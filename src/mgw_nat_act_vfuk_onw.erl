% VFUK-ONW specific mgw_nat actor callback functions

% (C) 2011 by Harald Welte <laforge@gnumonks.org>
% (C) 2011 OnWaves
%
% All Rights Reserved
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as
% published by the Free Software Foundation; either version 3 of the
% License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU Affero General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%
% Additional Permission under GNU AGPL version 3 section 7:
%
% If you modify this Program, or any covered work, by linking or
% combining it with runtime libraries of Erlang/OTP as released by
% Ericsson on http://www.erlang.org (or a modified version of these
% libraries), containing parts covered by the terms of the Erlang Public
% License (http://www.erlang.org/EPLICENSE), the licensors of this
% Program grant you additional permission to convey the resulting work
% without the need to license the runtime libraries of Erlang/OTP under
% the GNU Affero General Public License. Corresponding Source for a
% non-source form of such a combination shall include the source code
% for the parts of the runtime libraries of Erlang/OTP used as well as
% that of the covered work.

-module(mgw_nat_act_vfuk_onw).
-author("Harald Welte <laforge@gnumonks.org>").

-export([rewrite_actor/5, reload_config/0]).
-export([camelph_twalk_cb/3]).

-include_lib("osmo_map/include/map.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/isup.hrl").

% Seconds until the IMSI from an Update Location is removed from the
% cache if no matching Insert Subscriber Data was received
% before. This is the timer value "m" from GSM 09.02 17.1.2, as it is
% used for Update Location, specified in 17.6.1
-define(IMSI_TBL_TIMEOUT, 30).
% Minutes until the MO-SMS-CSI is removed from the database if it
% hasn't been renewed by another Insert Subscriber Data for the same
% IMSI
-define(CSI_TBL_TIMEOUT, 24 * 60).

% Rewrite at SCTP (root) level:
rewrite_actor(sctp, From, Path, 2, DataBin) ->
	try mgw_nat:mangle_rx_data(From, Path, DataBin, fun rewrite_actor/5) of
		Val ->
			Val
	catch error:Error ->
		% some parser error, simply forward msg unmodified
		error_logger:error_report([{error, Error},
					   {stacktrace, erlang:get_stacktrace()},
					   {from, From}, {path, Path},
					   {data_bin, DataBin}]),
		DataBin
	end;

% Rewrite at SCCP level: call into mangle_tt_sri_sm
rewrite_actor(sccp, from_msc, Path, SccpType, SccpDec) ->
	mangle_tt_sri_sm:mangle_tt_sri_sm(from_msc, Path, SccpType, SccpDec);

% Rewrite at MAP level: call into map_masq module
rewrite_actor(map, From, Path, 0, MapDec) ->
	NewMapDec = mangle_map_camel_phase(From, Path, MapDec),
	NewMapDec2 = mangle_map_subscriber_data(From, Path, NewMapDec),
	intercept_map_mo_forward_sm(From, Path, NewMapDec2);

% Default action: no rewrite
rewrite_actor(_Level, _From, _Path, _MsgType, Msg) ->
	Msg.

get_tid({_, MapPDU}) ->
	case MapPDU of
		#'Abort'{} ->
			{ false, MapPDU#'Abort'.dtid };
		#'MapSpecificPDUs_begin'{} ->
			{ MapPDU#'MapSpecificPDUs_begin'.otid, false };
		#'MapSpecificPDUs_end'{} ->
			{ false, MapPDU#'MapSpecificPDUs_end'.dtid };
		#'MapSpecificPDUs_continue'{} ->
			{ MapPDU#'MapSpecificPDUs_continue'.otid, MapPDU#'MapSpecificPDUs_continue'.dtid };
		_ ->
			{ false, false }
	end.

% Generic function to match SCCP Addr (AddrType) against a range in a
% table (RangeTblName) and if it matches modify message with the data
% from the table by calling TWalkFun
mangle_map_message(Path, AddrType, RangeTblName, MapDec, TWalkFun) ->
	% Resolve the Global Title of the SCCP Addr
	{value, #sccp_msg{parameters = SccpPars}} = lists:keysearch(sccp_msg, 1, Path),
	Addr = proplists:get_value(AddrType, SccpPars),
	{ok, Tbl} = application:get_env(mgw_nat, RangeTblName),
	case osmo_ss7_gtt:global_title_match(Tbl, Addr) of
		false ->
			MapDec;
		DataL ->
			#global_title{phone_number = PhoneNum} = Addr#sccp_addr.global_title,
			PhoneNumInt = osmo_util:digit_list2int(PhoneNum),
			Tids = get_tid(MapDec),
			io:format("Rewriting MAP message with ~p, GT ~p (TIDs: ~p)~n", [DataL, PhoneNumInt, Tids]),
			osmo_util:tuple_walk(MapDec, TWalkFun, [DataL, Addr, Tids])
	end.


mangle_map_camel_phase(from_stp, Path, MapDec) ->
	mangle_map_message(Path, calling_party_addr, int_camel_ph_tbl_outb, MapDec, fun camelph_twalk_outb_cb/3);
mangle_map_camel_phase(from_msc, Path, MapDec) ->
	mangle_map_message(Path, called_party_addr, int_camel_ph_tbl, MapDec, fun camelph_twalk_cb/3).

% structure of ETS imsi_tbl:
% key: consisting of tuple of original SCCP Calling Party Address and TCAP OTID
% value: IMSI (from LU)

% tuple tree walker callback function
camelph_twalk_outb_cb(['begin','MapSpecificPDUs_begin',basicROS,invoke,
		       'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'],
		       ULA = #'UpdateLocationArg'{}, [PhaseL, CallingPAddr, Tids | _]) ->
	Imsi = ULA#'UpdateLocationArg'.imsi,
	{Otid, _} = Tids,
	Imsi_key = {CallingPAddr, Otid},
	io:format("inserting into imsi table: key:~p val:~p~n",
		  [Imsi_key, Imsi]),
	true = ets:insert(imsi_tbl, {Imsi_key, Imsi}),
	timer:apply_after(timer:seconds(?IMSI_TBL_TIMEOUT), ets, match_delete, [imsi_tbl, {Imsi_key, '_'}]),
	delete_sd(Imsi),
	Vc = ULA#'UpdateLocationArg'.'vlr-Capability',
	Vc_out = case Vc of
			#'VLR-Capability'{} -> Vc;
			% Add VLR capabilities if none are present in UpdateLocationArg
			asn1_NOVALUE -> #'VLR-Capability'{}
		 end,
	% Manipulate the VLR capabilities in UpdateLocationArg
	ULA#'UpdateLocationArg'{'vlr-Capability' = Vc_out#'VLR-Capability'{supportedCamelPhases = PhaseL}};
camelph_twalk_outb_cb(_Path, Msg, _Args) ->
	% Default case: simply return the unmodified tuple
	Msg.

% tuple tree walker callback function
camelph_twalk_cb(['begin','MapSpecificPDUs_begin',basicROS,invoke,
		  'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke',
		  'UpdateLocationArg'], VC = #'VLR-Capability'{}, [PhaseL|_Args]) ->
	% Manipulate the VLR capabilities in UpdateLocationArg
	VC#'VLR-Capability'{supportedCamelPhases = PhaseL};
camelph_twalk_cb(_Path, Msg, _Args) ->
	% Default case: simply return the unmodified tuple
	Msg.


mangle_map_subscriber_data(from_stp, _Path, MapDec) ->
	MapDec;
mangle_map_subscriber_data(from_msc, Path, MapDec) ->
	mangle_map_message(Path, called_party_addr, int_camel_ph_tbl_outb, MapDec, fun subscr_data_twalk_cb/3).


% tuple tree walker callback function
subscr_data_twalk_cb(['continue', 'MapSpecificPDUs_continue', basicROS, invoke,
		      'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke' ],
		     ISD = #'InsertSubscriberDataArg'{'msisdn' = Msisdn},
		     [_PhaseL, CalledPAddr, Tids | _]) when Msisdn =/= asn1_NOVALUE ->
	{_, Dtid} = Tids,
	Imsi_key = {CalledPAddr, Dtid},
	save_sd(imsi_key, Imsi_key, msisdn, Msisdn),
	ISD;
subscr_data_twalk_cb(['continue', 'MapSpecificPDUs_continue', basicROS, invoke,
		      'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke', 'InsertSubscriberDataArg'],
		     #'VlrCamelSubscriptionInfo'{'mo-sms-CSI' = #'SMS-CSI'{} = MoSmsCsi},
		     [_PhaseL, CalledPAddr, Tids | _]) ->
	{_, Dtid} = Tids,
	Imsi_key = {CalledPAddr, Dtid},
	save_sd(imsi_key, Imsi_key, mosmscsi, MoSmsCsi),
	asn1_NOVALUE; % this might cause an empty ISD message
subscr_data_twalk_cb(['continue', 'MapSpecificPDUs_continue', basicROS, invoke,
		      'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke', 'InsertSubscriberDataArg'],
		     #'VlrCamelSubscriptionInfo'{}, _) ->
	asn1_NOVALUE; % this might cause an empty ISD message
subscr_data_twalk_cb(['begin','MapSpecificPDUs_begin',basicROS, invoke,
		      'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke' ],
		     ISD = #'InsertSubscriberDataArg'{'imsi' = Imsi,
						      'vlrCamelSubscriptionInfo' = #'VlrCamelSubscriptionInfo'{} = Csi},
		     _) when is_record(Csi#'VlrCamelSubscriptionInfo'.'mo-sms-CSI', 'SMS-CSI') ->
	save_sd(imsi, Imsi, mosmscsi, Csi#'VlrCamelSubscriptionInfo'.'mo-sms-CSI'),
	ISD#'InsertSubscriberDataArg'{'vlrCamelSubscriptionInfo' = asn1_NOVALUE}; % this might cause an empty ISD message
subscr_data_twalk_cb(['begin','MapSpecificPDUs_begin',basicROS, invoke,
		      'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke', 'InsertSubscriberDataArg'],
		     #'VlrCamelSubscriptionInfo'{}, _) ->
	asn1_NOVALUE; % this might cause an empty ISD message
subscr_data_twalk_cb(['begin','MapSpecificPDUs_begin',basicROS, invoke,
		      'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'],
		     DSD = #'DeleteSubscriberDataArg'{'imsi' = Imsi, 'camelSubscriptionInfoWithdraw' = 'NULL'}, _) ->
	delete_csi(Imsi),
	DSD#'DeleteSubscriberDataArg'{'camelSubscriptionInfoWithdraw' = asn1_NOVALUE}; % this might cause an empty DSD message
subscr_data_twalk_cb(['begin','MapSpecificPDUs_begin',basicROS, invoke,
		      'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'],
		     DSD = #'DeleteSubscriberDataArg'{'imsi' = Imsi, 'specificCSI-Withdraw' = CsiWithdraw },
		     _) when is_list(CsiWithdraw) ->
	case lists:member('mo-sms-csi', CsiWithdraw) of
		true -> delete_csi(Imsi);
		false -> dummy
	end,
	DSD#'DeleteSubscriberDataArg'{'specificCSI-Withdraw' = asn1_NOVALUE}; % this might cause an empty DSD message
subscr_data_twalk_cb(_Path, Msg, _Args) ->
	% Default case: simply return the unmodified tuple
	Msg.

%%%%%%%%%%%%%%%%%%%%% todo: create imsi_tbl at startup
save_sd(imsi_key, Imsi_key, Type, Val) ->
	case ets:lookup(imsi_tbl, Imsi_key) of
		[{_, Imsi}] ->
			save_sd(imsi, Imsi, Type, Val);
		[] ->
			io:format("SCCP addr/TCAP TID ~p not found~n", [Imsi_key])
	end;
save_sd(imsi, Imsi, Type, Val) ->
	case dets:lookup(csi_tbl, Imsi) of
		[{_, Msisdn, MoSmsCsi, Ts}] ->
			case Type of
				msisdn -> dets:insert(csi_tbl, {Imsi, Val, MoSmsCsi, Ts});
				mosmscsi -> dets:insert(csi_tbl, {Imsi, Msisdn, Val, Ts})
			end;
		[] ->
			Ts = now(),
			case Type of
				msisdn -> dets:insert(csi_tbl, {Imsi, Val, false, Ts});
				mosmscsi -> dets:insert(csi_tbl, {Imsi, false, Val, Ts})
			end
	end,
	timer:apply_after(timer:minutes(?CSI_TBL_TIMEOUT), dets, match_delete, [csi_tbl, {Imsi, '_', '_', Ts}]), 
	io:format("Saving Subscriber Data for imsi:~p ~p:~p~n", [Imsi, Type, Val]).

delete_sd(Imsi) ->
	dets:match_delete(csi_tbl, {Imsi, '_', '_', '_'}).

delete_csi(Imsi) ->
	save_sd(imsi, Imsi, mosmscsi, false).

%%%%%%%%%%%%%%%%%%%% todo: check old csi at startup time.

intercept_map_mo_forward_sm(from_msc, _Path, MapDec) ->
	MapDec;
intercept_map_mo_forward_sm(from_stp, Path, MapDec) ->
	mangle_map_message(Path, calling_party_addr, int_camel_ph_tbl_outb, MapDec, fun mo_sm_twalk_cb/3).

%%%%%%%%%%%%%%%%%% unfinished
mo_sm_twalk_cb(['begin', 'MapSpecificPDUs_begin', basicROS, invoke,
		'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke' ],
	       SMS = #'MO-ForwardSM-Arg'{'sm-RP-OA' = {msisdn, Msisdn}},
	       [_PhaseL, CallingPAddr | _]) ->
	case dets:match(csi_tbl, {'$1', Msisdn, '$2', '_'}) of
		[{Imsi, MoSmsCsi}] ->
			SMS;
		[] ->
			SMS
	end;
mo_sm_twalk_cb(_Path, Msg, _Args) ->
	Msg.


gen_int_camelph_tbl(L) ->
	gen_int_camelph_tbl(L, []).
gen_int_camelph_tbl([], Out) ->
	Out;
gen_int_camelph_tbl([{GttPart, PhasePart}|Tail], Out) ->
	GttMatch = osmo_ss7_gtt:'#new-gtt_match'(GttPart),
	% Fixme: use ordered insert!
	gen_int_camelph_tbl(Tail, Out ++ [{GttMatch, PhasePart}]).

convert_camelph_tbl(TblInName, TblOutName) ->
	{ok, CamelPatchTblIn} = application:get_env(mgw_nat, TblInName),
	io:format("VFUK-ONW actor: reloading config ~p~n", [CamelPatchTblIn]),
	try gen_int_camelph_tbl(CamelPatchTblIn) of
		TblOut ->
			application:set_env(mgw_nat, TblOutName, TblOut)
		catch error:Error ->
			error_logger:error_report([{error, Error},
						   {stacktrace, erlang:get_stacktrace()}])
	end.

reload_config() ->
	convert_camelph_tbl(camel_phase_patch_table, int_camel_ph_tbl),
	convert_camelph_tbl(camel_phase_patch_table_outbound, int_camel_ph_tbl_outb),
	{ok, CsiTblFilename} = application:get_env(mgw_nat, csi_tbl_filename),
	%%%%%%%%%%%%%%% todo: move dets table open to startup due to possible race condition
	dets:close(csi_tbl),
	{ok, _} = dets:open_file(csi_tbl, [{file, CsiTblFilename}]).
