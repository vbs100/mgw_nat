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

-export([rewrite_actor/5]).
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
		io:format("MGW NAT mangling Error: ~p~n", [Error]),
		DataBin
	end;

% Rewrite at MAP level: call into map_masq module
rewrite_actor(map, From, Path, 0, MapDec) ->
	mangle_map_camel_phase(From, Path, MapDec);

% Default action: no rewrite
rewrite_actor(_Level, _From, _Path, _MsgType, Msg) ->
	Msg.


mangle_map_camel_phase(from_stp, Path, MapDec) ->
	MapDec;
mangle_map_camel_phase(from_msc, Path, MapDec) ->
	% Resolve the Global Title of the SCCP Called Addr
	#sccp_msg{parameters = SccpPars} = lists:keyfind(sccp_msg, 1, Path),
	CalledAddr = proplists:get_value(called_party_addr, SccpPars),
	#global_title{phone_number = PhoneNum} = CalledAddr#sccp_addr.global_title,
	PhoneNumInt = osmo_util:digit_list2int(PhoneNum),
	{ok, CamelPatchTbl} = application:get_env(camel_phase_patch_table),
	case lists:keyfind(PhoneNumInt, 1, CamelPatchTbl) of
		false ->
			MapDec;
		{ _Num, PhaseL } ->
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
