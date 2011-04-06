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

-module(mgw_nat_act_vfuk_onw).
-author("Harald Welte <laforge@gnumonks.org>").

-export([rewrite_actor/5, reload_config/0]).
-export([camelph_twalk_cb/3]).

-include_lib("osmo_map/include/map.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").

% Rewrite at SCTP (root) level:
rewrite_actor(sctp, From, Path, 2, DataBin) ->
	try mgw_nat:mangle_rx_data(From, Path, DataBin, fun rewrite_actor/5) of
		Val ->
			Val
	catch error:Error ->
		% some parser error, simply forward msg unmodified
		error_logger:error_report([{error, Error},
					   {stacktrace, erlang:get_stacktrace()}]),
		DataBin
	end;

% Rewrite at MAP level: call into map_masq module
rewrite_actor(map, From, Path, 0, MapDec) ->
	mangle_map_camel_phase(From, Path, MapDec);

% Default action: no rewrite
rewrite_actor(_Level, _From, _Path, _MsgType, Msg) ->
	Msg.


mangle_map_camel_phase(from_stp, _Path, MapDec) ->
	MapDec;
mangle_map_camel_phase(from_msc, Path, MapDec) ->
	% Resolve the Global Title of the SCCP Called Addr
	{value, #sccp_msg{parameters = SccpPars}} = lists:keysearch(sccp_msg, 1, Path),
	CalledAddr = proplists:get_value(called_party_addr, SccpPars),
	{ok, IntTbl} = application:get_env(mgw_nat, int_camel_ph_tbl),
	case osmo_ss7_gtt:global_title_match(IntTbl, CalledAddr) of
		false ->
			MapDec;
		PhaseL ->
			#global_title{phone_number = PhoneNum} = CalledAddr#sccp_addr.global_title,
			PhoneNumInt = osmo_util:digit_list2int(PhoneNum),
			io:format("Rewriting Camel Phase List to ~p, GT ~p~n", [PhaseL, PhoneNumInt]),
			osmo_util:tuple_walk(MapDec, fun camelph_twalk_cb/3, [PhaseL])
	end.


% tuple tree walker callback function
camelph_twalk_cb(['begin','MapSpecificPDUs_begin',basicROS,invoke,
		  'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke',
		  'UpdateLocationArg'], VC = #'VLR-Capability'{}, [PhaseL|_Args]) ->
	% Manipulate the VLR capabilities in UpdateLocationArg
	VC#'VLR-Capability'{supportedCamelPhases = PhaseL};
camelph_twalk_cb(_Path, Msg, _Args) ->
	% Default case: simply return the unmodified tuple
	Msg.


gen_int_camelph_tbl(L) ->
	gen_int_camelph_tbl(L, []).
gen_int_camelph_tbl([], Out) ->
	Out;
gen_int_camelph_tbl([{GttPart, PhasePart}|Tail], Out) ->
	GttMatch = osmo_ss7_gtt:'#new-gtt_match'(GttPart),
	% Fixme: use ordered insert!
	gen_int_camelph_tbl(Tail, Out ++ [{GttMatch, PhasePart}]).

reload_config() ->
	{ok, CamelPatchTblIn} = application:get_env(mgw_nat, camel_phase_patch_table),
	io:format("VFUK-ONW actor: reloading config ~p~n", [CamelPatchTblIn]),
	try gen_int_camelph_tbl(CamelPatchTblIn) of
		TblOut ->
			application:set_env(mgw_nat, int_camel_ph_tbl, TblOut)
		catch error:Error ->
			error_logger:error_report([{error, Error},
						   {stacktrace, erlang:get_stacktrace()}])
	end.
