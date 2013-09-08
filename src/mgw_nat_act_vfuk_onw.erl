% VFUK-ONW specific mgw_nat actor callback functions

% (C) 2011-2013 by Harald Welte <laforge@gnumonks.org>
% (C) 2011-2013 OnWaves
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
-author("Tobias Engel <tobias@sternraute.de>").

-export([rewrite_actor/5, reload_config/0, init_config/0]).
-export([camelph_twalk_cb/3, camelph_twalk_outb_cb/3, subscr_data_twalk_cb/3,
	 mo_sm_twalk_cb/3, mo_sm_resp_twalk_cb/3]).

-include_lib("TCAP/include/TCAPMessages.hrl").
-include_lib("osmo_map/include/map.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/isup.hrl").

%%%
%%% Rewrite actors for different layers of a message
%%%

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
% Also: intercept MO-FORWARD-SM and corresponding responses for the fake SMSSSF
rewrite_actor(map, From, Path, 0, MapDec) ->
	MapDec2 = mangle_map_camel_phase(From, Path, MapDec),
	MapDec3 = mangle_map_subscriber_data(From, Path, MapDec2),
	MapDec4 = mangle_isd_call_barr(From, Path, MapDec3),
	MapDec5 = intercept_map_mo_forward_sm(From, Path, MapDec4),
	intercept_map_mo_forward_sm_resp(From, Path, MapDec5);

% Default action: no rewrite
rewrite_actor(_Level, _From, _Path, _MsgType, Msg) ->
	Msg.


%%%
%%% Handle Location Updates
%%%

%% mangle_map_camel_phase
%%
%% Add (to messages from VPLMN for outbound roaming subsribers) or
%% remove (from messages from ONW MSC for inbound roaming subscribers)
%% CAMEL Phase IEs according to table (int_camel_ph_tbl_outb /
%% int_camel_ph_tbl)

mangle_map_camel_phase(from_stp, Path, MapDec) ->
	mangle_map_message(Path, calling_party_addr, int_camel_ph_tbl_outb,
			   MapDec, fun camelph_twalk_outb_cb/3);
mangle_map_camel_phase(from_msc, Path, MapDec) ->
	mangle_map_message(Path, called_party_addr, int_camel_ph_tbl,
			   MapDec, fun camelph_twalk_cb/3).


%% camelph_twalk_outb_cb
%%
%% Add VLR-Capability with supportedCamelPhases for outbound roaming
%% subscribers. Also, save the IMSI of the subscriber in ETS table
%% imsi_tbl (key consisting of tuple of original SCCP Calling Party
%% Address and TCAP OTID)

camelph_twalk_outb_cb(['begin', 'MapSpecificPDUs_begin', basicROS, invoke,
		       'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'],
		      ULA = #'UpdateLocationArg'{},
		      [PhaseL, CallingPAddr, {Otid, _} | _]) ->
	Imsi = ULA#'UpdateLocationArg'.imsi,
	Imsi_key = {CallingPAddr, Otid},
	io:format("inserting into imsi table: key:~p val:~p~n",
		  [Imsi_key, Imsi]),
	ets:insert(imsi_tbl, {Imsi_key, Imsi}),
	{ok, ImsiTblTimeout} = application:get_env(mgw_nat, imsi_tbl_timeout_secs),
	ets_delete_after(timer:seconds(ImsiTblTimeout), imsi_tbl, Imsi_key),
	delete_sd(Imsi),
	Vc = ULA#'UpdateLocationArg'.'vlr-Capability',
	Vc_out = case Vc of
			#'VLR-Capability'{} -> Vc;
			% Add VLR capabilities if none are present in
			% UpdateLocationArg
			asn1_NOVALUE -> #'VLR-Capability'{}
		 end,
	% Manipulate the VLR capabilities in UpdateLocationArg
	Vc_out2 = Vc_out#'VLR-Capability'{supportedCamelPhases = PhaseL},
	ULA#'UpdateLocationArg'{'vlr-Capability' = Vc_out2};
%% Camel phases are also indicated in the returnResult to the InsertSubscriberData,
%% where we need to patch them accordingly.
camelph_twalk_outb_cb(['continue', 'MapSpecificPDUs_continue', basicROS, returnResult,
		       'MapSpecificPDUs_continue_components_SEQOF_basicROS_returnResult',
		       'MapSpecificPDUs_continue_components_SEQOF_basicROS_returnResult_result'],
		      IsdRes = #'InsertSubscriberDataRes'{},
		      [PhaseL, CallingPAddr, {Otid, _} | _]) ->
	IsdRes#'InsertSubscriberDataRes'{supportedCamelPhases = PhaseL};
camelph_twalk_outb_cb(['end', 'MapSpecificPDUs_end', basicROS, returnResult,
		       'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult',
		       'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result'],
		      IsdRes = #'InsertSubscriberDataRes'{},
		      [PhaseL, CallingPAddr, {Otid, _} | _]) ->
	IsdRes#'InsertSubscriberDataRes'{supportedCamelPhases = PhaseL};

camelph_twalk_outb_cb(_Path, Msg, _Args) ->
	% Default case: simply return the unmodified tuple
	Msg.


%% camelph_twalk_cb
%%
%% Remove VLR-Capability for inbound roaming subscribers

camelph_twalk_cb(['begin','MapSpecificPDUs_begin',basicROS,invoke,
		  'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke',
		  'UpdateLocationArg'],
		 VC = #'VLR-Capability'{},
		 [PhaseL|_Args]) ->
	% Manipulate the VLR capabilities in UpdateLocationArg
	VC#'VLR-Capability'{supportedCamelPhases = PhaseL};

camelph_twalk_cb(_Path, Msg, _Args) ->
	% Default case: simply return the unmodified tuple
	Msg.


%%%
%%% Handle subscriber data related messages
%%%

% Rewrite callBarringInfo in InsertSubscriberDataArg HLR->VLR
mangle_isd_call_barr(from_msc, _Path, MapDec) ->
	osmo_util:tuple_walk(MapDec, fun mangle_callbarr:callbarr_twalk_cb/3, []);
mangle_isd_call_barr(_From, _Path, MapDec) ->
	MapDec.

%% mangle_map_subscriber_data
%%
%% Process MAP messages coming from ONW MSC. Consult table for
%% outbound roaming subscribers (int_camel_ph_tbl_outb) to decide if
%% message should be modified

mangle_map_subscriber_data(from_stp, _Path, MapDec) ->
	MapDec;
mangle_map_subscriber_data(from_msc, Path, MapDec) ->
	mangle_map_message(Path, called_party_addr, int_camel_ph_tbl_outb,
			   MapDec, fun subscr_data_twalk_cb/3).


%% subscr_data_twalk_cb
%%
%% Save or delete subscriber data from ISD (embedded or standalone)
%% and DSD coming from ONW MSC. Also, remove CAMEL related IEs.

% Save MSISDN from embedded ISD
subscr_data_twalk_cb(['continue', 'MapSpecificPDUs_continue', basicROS, invoke,
		      'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'],
		     ISD = #'InsertSubscriberDataArg'{'msisdn' = Msisdn},
		     [_PhaseL, CalledPAddr, Tids | _])
    when Msisdn =/= asn1_NOVALUE ->
	{_, Dtid} = Tids,
	Imsi_key = {CalledPAddr, Dtid},
	save_sd(imsi_key, Imsi_key, msisdn, Msisdn),
	ISD;

% Save MO-SMS-CSI from embedded ISD and remove VlrCamelSubscriptionInfo
subscr_data_twalk_cb(['continue', 'MapSpecificPDUs_continue', basicROS, invoke,
		      'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke', 'InsertSubscriberDataArg'],
		     #'VlrCamelSubscriptionInfo'{'mo-sms-CSI' = #'SMS-CSI'{} = MoSmsCsi},
		     [_PhaseL, CalledPAddr, Tids | _]) ->
	{_, Dtid} = Tids,
	Imsi_key = {CalledPAddr, Dtid},
	save_sd(imsi_key, Imsi_key, mosmscsi, MoSmsCsi),
	% this might cause an empty ISD message
	asn1_NOVALUE;

% If there is no MO-SMS-CSI in VlrCamelSubscriptionInfo just remove it
subscr_data_twalk_cb(['continue', 'MapSpecificPDUs_continue', basicROS, invoke,
		      'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke',
		      'InsertSubscriberDataArg'],
		     #'VlrCamelSubscriptionInfo'{}, _) ->
	% this might cause an empty ISD message
	asn1_NOVALUE;

% Save MO-SMS-CSI from standalone ISD and remove
% VlrCamelSubscriptionInfo (MSISDN should still be saved from LU with
% embedded ISD)
subscr_data_twalk_cb(['begin','MapSpecificPDUs_begin',basicROS, invoke,
		      'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'],
		     ISD = #'InsertSubscriberDataArg'{'imsi' = Imsi,
						      'vlrCamelSubscriptionInfo' = #'VlrCamelSubscriptionInfo'{} = Csi},
		     _)
    when is_record(Csi#'VlrCamelSubscriptionInfo'.'mo-sms-CSI', 'SMS-CSI') ->
	save_sd(imsi, Imsi, mosmscsi,
		Csi#'VlrCamelSubscriptionInfo'.'mo-sms-CSI'),
	% this might cause an empty ISD message
	ISD#'InsertSubscriberDataArg'{'vlrCamelSubscriptionInfo' = asn1_NOVALUE};

% If there is no MO-SMS-CSI in VlrCamelSubscriptionInfo just remove it
subscr_data_twalk_cb(['begin','MapSpecificPDUs_begin',basicROS, invoke,
		      'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke',
		      'InsertSubscriberDataArg'],
		     #'VlrCamelSubscriptionInfo'{}, _) ->
	% this might cause an empty DSD message
	asn1_NOVALUE;

% Delete MO-SMS-CSI and remove camelSubscriptionInfoWithdraw IE
subscr_data_twalk_cb(['begin','MapSpecificPDUs_begin',basicROS, invoke,
		      'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'],
		     DSD = #'DeleteSubscriberDataArg'{'imsi' = Imsi,
						      'camelSubscriptionInfoWithdraw' = 'NULL'},
		     _) ->
	delete_csi(Imsi),
	DSD#'DeleteSubscriberDataArg'{'camelSubscriptionInfoWithdraw' = asn1_NOVALUE};

% Delete MO-SMS-CSI and remove specificCSI-Withdraw IE
subscr_data_twalk_cb(['begin','MapSpecificPDUs_begin',basicROS, invoke,
		      'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'],
		     DSD = #'DeleteSubscriberDataArg'{'imsi' = Imsi,
						      'specificCSI-Withdraw' = CsiWithdraw },
		     _)
    when is_list(CsiWithdraw) ->
	case lists:member('mo-sms-csi', CsiWithdraw) of
		true -> delete_csi(Imsi);
		_ -> ok
	end,
	% this might cause an empty DSD message
	DSD#'DeleteSubscriberDataArg'{'specificCSI-Withdraw' = asn1_NOVALUE};

subscr_data_twalk_cb(_Path, Msg, _Args) ->
	% Default case: simply return the unmodified tuple
	Msg.


%%%
%%% Handle MO SMS
%%%

%% intercept_map_mo_forward_sm
%%
%% Intercept MO-FORWARD-SM from VPLMN. Consult table for outbound
%% roaming subscribers (int_camel_ph_tbl_outb) to decide if message
%% should be intercepted.

intercept_map_mo_forward_sm(from_msc, _Path, MapDec) ->
	MapDec;
intercept_map_mo_forward_sm(from_stp, Path, MapDec) ->
	mangle_map_message(Path, calling_party_addr, int_camel_ph_tbl_outb,
			   MapDec, fun mo_sm_twalk_cb/3).


%% mo_sm_twalk_cb
%%
%% Tuplewalk function to handle a MO-FORWARD-SM from the VPLMN. This
%% can either be a TCAP Begin oder Continue (in case of TCAP
%% handshake) or an Abort. All the relevant original data gets passed
%% to a new fake_msc process, then the "stolen" exception is thrown to
%% signal that this message should be discarded in this process.

% SMS in Begin
mo_sm_twalk_cb(['begin', 'MapSpecificPDUs_begin', basicROS, invoke],
	       Inv = #'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{invokeId = {present, InvokeId},
									       argument = SMS = #'MO-ForwardSM-Arg'{'sm-RP-OA' = {msisdn, Msisdn}}},
	       [_PhaseL, CallingPAddr, {Otid, _}, Path, MapDec | _]) ->
	% find CSI by MSISDN
	case dets:match(csi_tbl, {'$1', Msisdn, '$2', '_'}) of
		[[Imsi, MoSmsCsi]] ->
			MPid_key = {CallingPAddr, Otid},
			{ok, MPid} = supervisor:start_child(fake_msc_sup,
							    [[Imsi, Msisdn,
							      MoSmsCsi, SMS, 
							      InvokeId, Path,
							      MapDec]]),
			ets:insert(mpid_tbl, {MPid_key, MPid}),
			{ok, MPidTblTimeout} = application:get_env(mgw_nat,
								   mpid_tbl_timeout_secs),
			ets_delete_after(timer:seconds(MPidTblTimeout),
					 mpid_tbl,
					 MPid_key),
			throw(stolen);
		[] ->
			Inv % fixme: release sms if CSI not found?
	end;

% SMS in Continue after TCAP handshake
mo_sm_twalk_cb(['continue', 'MapSpecificPDUs_continue', basicROS, invoke],
	       Inv = #'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{invokeId = {present, InvokeId},
										  argument = SMS = #'MO-ForwardSM-Arg'{'sm-RP-OA' = {msisdn, Msisdn}}},
	       [_PhaseL, CallingPAddr, {Otid, Dtid}, Path, MapDec | _]) ->
	% find CSI by MSISDN
	case dets:match(csi_tbl, {'$1', Msisdn, '$2', '_'}) of
		[{Imsi, MoSmsCsi}] ->
			MPid_key = {CallingPAddr, Otid},
			{ok, MPid} = supervisor:start_child(fake_msc_sup,
							    [[Imsi, Msisdn,
							      MoSmsCsi, SMS,
							      InvokeId, Path,
							      MapDec]]),
			% This also saves the DTID so the dialogue
			% handler can be found in case of an abort
			ets:insert(mpid_tbl, {MPid_key, MPid, Dtid}),
			{ok, MPidTblTimeout} = application:get_env(mgw_nat,
								   mpid_tbl_timeout_secs),
			ets_delete_after(timer:seconds(MPidTblTimeout),
					 mpid_tbl,
					 MPid_key),
			throw(stolen);
		[] ->
			Inv % fixme: release sms if CSI not found?
	end;

% fixme: this might be processed out of order if the original message
% has not been sent on yet
% Abort from VPLMN can only be sent after the dialogue has been
% established, so only in the case of a TCAP handshake
mo_sm_twalk_cb(['abort'],
	       Msg = #'Abort'{},
	       [_PhaseL, CallingPAddr, {_, Dtid} | _]) ->
	MPid_key = {CallingPAddr, Dtid},
	case find_dialogue_handler(dtid, MPid_key) of
		{ok, MPid} -> fake_msc:camel_o_sms_failure(MPid);
		_ -> ok
	end,
	Msg;

mo_sm_twalk_cb(_Path, Msg, _Args) ->
	Msg.


%%%
%%% Handle MO SMS responses
%%%

%% intercept_map_mo_forward_sm_resp
%%
%% Intercept MO-FORWARD-SM response from ONW MSC. Consult table for
%% outbound roaming subscribers (int_camel_ph_tbl_outb) to decide if
%% message should be intercepted.

intercept_map_mo_forward_sm_resp(from_msc, Path, MapDec) ->
	mangle_map_message(Path, called_party_addr, int_camel_ph_tbl_outb,
			   MapDec, fun mo_sm_resp_twalk_cb/3);

intercept_map_mo_forward_sm_resp(from_stp, _Path, MapDec) ->
	MapDec.


%% mo_sm_resp_twalk_cb
%%
%% Tuplewalk function to handle a MO-FORWARD-SM response from ONW
%% MSC. This can only be a TCAP End (or Abort), since there is no
%% "More Messages to Send" field for MO SMS. The fake SSF gets
%% notified and the message is passed on unmodified in all cases.

% outbound (from msc) tcap end, Dtid == originators Otid
mo_sm_resp_twalk_cb(['end', 'MapSpecificPDUs_end', basicROS, returnResult],
		   Msg,	[_PhaseL, CalledPAddr, {_, Dtid} | _]) ->
	MPid_key = {CalledPAddr, Dtid},
	case find_dialogue_handler(otid, MPid_key) of
		{ok, MPid} -> fake_msc:camel_o_sms_submitted(MPid);
		_ -> ok
	end,
	Msg;

mo_sm_resp_twalk_cb(['end', 'MapSpecificPDUs_end', basicROS, returnError],
		    Msg = #'MapSpecificPDUs_end_components_SEQOF_basicROS_returnError'{errcode = {local, Errcode}},
		    [_PhaseL, CalledPAddr, {_, Dtid} | _]) ->
	MPid_key = {CalledPAddr, Dtid},
	case find_dialogue_handler(otid, MPid_key) of
		{ok, MPid} -> fake_msc:camel_o_sms_failure(MPid, Errcode);
		_ -> ok
	end,
	Msg;

mo_sm_resp_twalk_cb(['abort'],
		    Msg = #'Abort'{},
		    [_PhaseL, CalledPAddr, {_, Dtid} | _]) ->
	MPid_key = {CalledPAddr,Dtid},
	case find_dialogue_handler(otid, MPid_key) of
		{ok, MPid} -> fake_msc:camel_o_sms_failure(MPid);
		_ -> ok
	end,
	Msg;

mo_sm_resp_twalk_cb(_Path, Msg, _Args) ->
	Msg.


%%%
%%% Helper functions
%%%

%% save_sd
%%
%% Stores IMSI, MSISDN and MO-SMS-CSI of a subscriber in csi_tbl DETS
%% table. If IMSI is not available from the request (during embedded
%% ISD) it will be looked up in imsi_tbl ETS table by SCCP Calling
%% Party Address and TCAP OTID. Timestamp of insertion time gets also
%% saved so the data can be safely expired after a certain amount of
%% time without renewal.

save_sd(imsi_key, Imsi_key, Type, Val) ->
	case ets:lookup(imsi_tbl, Imsi_key) of
		[{_, Imsi}] ->
			save_sd(imsi, Imsi, Type, Val);
		[] ->
			io:format("SCCP addr/TCAP TID ~p not found~n",
				  [Imsi_key])
	end;

save_sd(imsi, Imsi, Type, Val) ->
	case dets:lookup(csi_tbl, Imsi) of
		[{_, Msisdn, MoSmsCsi, Ts}] ->
			case Type of
				msisdn -> dets:insert(csi_tbl,
						      {Imsi, Val, MoSmsCsi, Ts});
				mosmscsi -> dets:insert(csi_tbl,
							{Imsi, Msisdn, Val, Ts})
			end;
		[] ->
			Ts = now(),
			case Type of
				msisdn -> dets:insert(csi_tbl,
						      {Imsi, Val, false, Ts});
				mosmscsi -> dets:insert(csi_tbl,
							{Imsi, false, Val, Ts})
			end
	end,
	{ok, CsiTblTimeout} = application:get_env(mgw_nat,
						  csi_tbl_timeout_mins),
	dets_match_delete_after(timer:minutes(CsiTblTimeout),
				csi_tbl,
				{Imsi, '_', '_', Ts}), 
	io:format("Saving Subscriber Data for imsi:~p ~p:~p~n",
		  [Imsi, Type, Val]).


%% delete_sd
%%
%% Removes subscriber data from csi_tbl by IMSI.

delete_sd(Imsi) ->
	dets:delete(csi_tbl, Imsi).


%% delete_csi
%%
%% Removes only MO-SMS-CSI data frmo csi_tb for subscriber identified
%% by IMSI.

delete_csi(Imsi) ->
	save_sd(imsi, Imsi, mosmscsi, false).


%% dets_match_delete_after

dets_match_delete_after(Time, Name, Pattern) ->
	io:format("*** deleting pattern ~p from table ~p after ~p~n",
		  [Pattern, Name, Time]),
    	timer:apply_after(Time, dets, match_delete, [Name, Pattern]).


%% ets_delete_after

ets_delete_after(Time, Tab, Key) ->
	io:format("*** deleting key ~p from table ~p after ~p~n",
		  [Key, Tab, Time]),
	timer:apply_after(Time, ets, delete, [Tab, Key]).


%% find_dialogue_handler
%%
%% In the mpid_tbl table, lookup the PID of the fake_msc process that
%% handles this MO-FORWARD-SM TCAP dialogue. Removes the PID from the
%% table and returns it.

find_dialogue_handler(otid, MPid_key) ->
	Ret = case ets:lookup(mpid_tbl, MPid_key) of
		  [{_, MPid}] -> {ok, MPid};
		  [] -> notfound
	      end,
	ets:delete(mpid_tbl, MPid_key),
	Ret;

find_dialogue_handler(dtid, {CPAddr, Dtid}) ->
	case ets:match_delete(mpid_tbl, {{CPAddr, '_'}, '$1', Dtid}) of
		[[MPid]] -> {ok, MPid};
		[] -> notfound
	end.


%% get_tid
%%
%% Return the TIDs present in a TCAP message

get_tid({_, MapPDU}) ->
	case MapPDU of
		#'Abort'{} ->
			{ false, MapPDU#'Abort'.dtid };
		#'MapSpecificPDUs_begin'{} ->
			{ MapPDU#'MapSpecificPDUs_begin'.otid, false };
		#'MapSpecificPDUs_end'{} ->
			{ false, MapPDU#'MapSpecificPDUs_end'.dtid };
		#'MapSpecificPDUs_continue'{} ->
			{ MapPDU#'MapSpecificPDUs_continue'.otid,
			  MapPDU#'MapSpecificPDUs_continue'.dtid };
		_ ->
			{ false, false }
	end.


%% mangle_map_message
%%
%% Generic function to match an SCCP Address of type AddrType (called_
%% or calling_party_addr) against a range in a table (RangeTblName)
%% and if it matches modify message with the data from the table by
%% calling TWalkFun

mangle_map_message(Path, AddrType, RangeTblName, MapDec, TWalkFun) ->
	% Resolve the Global Title of the SCCP Addr
	{value, #sccp_msg{parameters = SccpPars}} = lists:keysearch(sccp_msg,
								    1,
								    Path),
	Addr = proplists:get_value(AddrType, SccpPars),
	{ok, Tbl} = application:get_env(mgw_nat, RangeTblName),
	case osmo_ss7_gtt:global_title_match(Tbl, Addr) of
		false ->
			MapDec;
		DataL ->
			#global_title{phone_number = PhoneNum} = Addr#sccp_addr.global_title,
			PhoneNumInt = osmo_util:digit_list2int(PhoneNum),
			Tids = get_tid(MapDec),
			io:format("Rewriting MAP message with ~p, GT ~p (TIDs: ~p)~n",
				  [DataL, PhoneNumInt, Tids]),
			osmo_util:tuple_walk(MapDec,
					     TWalkFun,
					     [DataL, Addr, Tids, Path, MapDec])
	end.


%%%
%%% Config related functions
%%%

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
			error_logger:error_report([{error,
						    Error},
						   {stacktrace,
						    erlang:get_stacktrace()}])
	end.


init_config() ->
	ets:new(imsi_tbl, [named_table, public]),
	ets:new(mpid_tbl, [named_table, public]),
	{ok, CsiTblFilename} = application:get_env(mgw_nat, csi_tbl_filename),
	% fixme: do we need to close the table during shutdown?
	{ok, _} = dets:open_file(csi_tbl, [{file, CsiTblFilename}]),
	% restart timers for subscriber data timeout
	{ok, CsiTblTimeout} = application:get_env(mgw_nat,
						  csi_tbl_timeout_mins),
	TimerVal = timer:minutes(CsiTblTimeout),
	TravFun = fun({Key, _, _, Ts}) ->
			  dets_match_delete_after(TimerVal,
						  csi_tbl,
						  {Key, '_', '_', Ts}),
			  continue
		  end,
	dets:traverse(csi_tbl, TravFun).


reload_config() ->
	convert_camelph_tbl(camel_phase_patch_table,
			    int_camel_ph_tbl),
	convert_camelph_tbl(camel_phase_patch_table_outbound,
			    int_camel_ph_tbl_outb).
