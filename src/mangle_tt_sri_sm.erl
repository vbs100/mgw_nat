% FIXME

% (C) 2012 by Harald Welte <laforge@gnumonks.org>
% (C) 2012 OnWaves
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

-module(mangle_tt_sri_sm).
-author("Harald Welte <laforge@gnumonks.org>").

-export([mangle_tt_sri_sm/4]).

-export([gt_match_pfx/2, gt_match_pfx_list/2,
	 isup_party_match_pfx/2, isup_party_match_pfx_list/2]).

-export([get_tcap_components/1, get_tcap_operation/1, get_tcap_operations/1,
	 check_for_tcap_op/3, check_for_invoke_sri_sm/1]).

-include_lib("osmo_map/include/map.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/isup.hrl").

% high-level function to determine if a Sccp / MAP message contains a Invoke SRI-SM
check_for_invoke_sri_sm(MapDec) ->
	check_for_tcap_op(invoke, {local, 45}, MapDec).


% check if there's a prefix match between a given GT and prefix
gt_match_pfx(GT, MatchPfx) when is_record(GT, global_title),
				is_integer(MatchPfx) ->
	gt_match_pfx(GT, osmo_util:int2digit_list(MatchPfx));
gt_match_pfx(GT, MatchPfx) when is_record(GT, global_title),
				is_list(MatchPfx) ->
	match_pfx(GT#global_title.phone_number, MatchPfx).

% check if there's a prefix match between a given ISUP party_addr and prefix
isup_party_match_pfx(Party, MatchPfx) when is_record(Party, party_number),
	       				   is_integer(MatchPfx)	->
	isup_party_match_pfx(Party, osmo_util:int2digit_list(MatchPfx));
isup_party_match_pfx(Party, MatchPfx) when is_record(Party, party_number) ->
	DigitsIn = Party#party_number.phone_number,
	match_pfx(DigitsIn, MatchPfx).

match_pfx(DigitsIn, MatchPfx) when is_list(DigitsIn), is_list(MatchPfx) ->
	MatchPfxLen = length(MatchPfx),
	Pfx = lists:sublist(DigitsIn, 1, MatchPfxLen),
	case Pfx of
		MatchPfx ->
			true;
		_ ->
			false
	end.

% check if there's a prefix match of Global Titles among a list of prefixes
gt_match_pfx_list(GT, []) when is_record(GT, global_title) ->
	false;
gt_match_pfx_list(GT, [MatchPfx|Tail]) when is_record(GT, global_title) ->
	case gt_match_pfx(GT, MatchPfx) of
		true ->
			true;
		_ ->
			gt_match_pfx_list(GT, Tail)
	end.

% check if there's a prefix match of ISUP Party number among a list of prefixes
isup_party_match_pfx_list(PN, []) when is_record(PN, party_number) ->
	false;
isup_party_match_pfx_list(PN, [MatchPfx|Tail]) when is_record(PN, party_number) ->
	case isup_party_match_pfx(PN, MatchPfx) of
		true ->
			true;
		_ ->
			isup_party_match_pfx_list(PN, Tail)
	end.

% get a list of components from the decoded TCAP+MAP nested record
get_tcap_components({'begin', Beg}) ->
	get_tcap_components(Beg);
get_tcap_components({'end', Beg}) ->
	get_tcap_components(Beg);
get_tcap_components({'continue', Beg}) ->
	get_tcap_components(Beg);
% map.erl
get_tcap_components(#'MapSpecificPDUs_begin'{components=Comps}) ->
	Comps;
get_tcap_components(#'MapSpecificPDUs_continue'{components=Comps}) ->
	Comps;
get_tcap_components(#'MapSpecificPDUs_end'{components=Comps}) ->
	Comps;
get_tcap_components(_) ->
	[].

% get the MAP operation of a specific component
get_tcap_operation({basicROS, Rec}) ->
	get_tcap_operation(Rec);
get_tcap_operation({invoke, Rec}) ->
	get_tcap_operation(Rec);
get_tcap_operation({returnResult, Rec}) ->
	get_tcap_operation(Rec);
get_tcap_operation({returnResultNotLast, Rec}) ->
	get_tcap_operation(Rec);
% map.erl
get_tcap_operation(#'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{opcode=Op}) ->
	{invoke, Op};
get_tcap_operation(#'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{opcode=Op}) ->
	{invoke, Op};
get_tcap_operation(#'MapSpecificPDUs_end_components_SEQOF_basicROS_invoke'{opcode=Op}) ->
	{invoke, Op};
get_tcap_operation(#'MapSpecificPDUs_begin_components_SEQOF_basicROS_returnResult'{result=Res}) ->
	{returnResult, Res#'MapSpecificPDUs_begin_components_SEQOF_basicROS_returnResult_result'.opcode};
get_tcap_operation(#'MapSpecificPDUs_continue_components_SEQOF_basicROS_returnResult'{result=Res}) ->
	{returnResult, Res#'MapSpecificPDUs_continue_components_SEQOF_basicROS_returnResult_result'.opcode};
get_tcap_operation(#'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult'{result=Res}) ->
	{returnResult, Res#'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result'.opcode};
get_tcap_operation(#'MapSpecificPDUs_begin_components_SEQOF_returnResultNotLast'{result=Res}) ->
	{returnResult, Res#'MapSpecificPDUs_begin_components_SEQOF_returnResultNotLast_result'.opcode};
get_tcap_operation(#'MapSpecificPDUs_continue_components_SEQOF_returnResultNotLast'{result=Res}) ->
	{returnResult, Res#'MapSpecificPDUs_continue_components_SEQOF_returnResultNotLast_result'.opcode};
get_tcap_operation(#'MapSpecificPDUs_end_components_SEQOF_returnResultNotLast'{result=Res}) ->
	{returnResult, Res#'MapSpecificPDUs_end_components_SEQOF_returnResultNotLast_result'.opcode}.

% get a list of the MAP operations inside the components of a MAP message
get_tcap_operations(MapDec) ->
	Comps = get_tcap_components(MapDec),
	[get_tcap_operation(X) || X <- Comps].


check_for_tcap_op(Comp, Op, SccpDec) when is_record(SccpDec, sccp_msg) ->
	UserData = proplists:get_value(user_data, SccpDec#sccp_msg.parameters),
	MapDec = map_codec:parse_tcap_msg(UserData),
	check_for_tcap_op(Comp, Op, MapDec);

check_for_tcap_op(Comp, Op, MapDec) ->
	MapOps = get_tcap_operations(MapDec),
	% check for invoke of SRI-for-SM:
	lists:member({Comp, Op}, MapOps).


mangle_tt_sri_sm(from_msc, _Path, ?SCCP_MSGT_UDT, SccpDec = #sccp_msg{parameters=Opts}) ->
	CalledParty = proplists:get_value(called_party_addr, Opts),
	CalledGT = CalledParty#sccp_addr.global_title,
	case application:get_env(mgw_nat, mangle_tt_sri_sm_pfx) of
	    {ok, PrefixList} ->
		case gt_match_pfx_list(CalledGT, PrefixList) of
		    true ->
			case check_for_invoke_sri_sm(SccpDec) of
			    true ->
				CalledGTNew = CalledGT#global_title{trans_type = 3},
				CalledPartyNew = CalledParty#sccp_addr{global_title = CalledGTNew},
				ParamsOut = lists:keyreplace(called_party_addr, 1, Opts,
							     {called_party_addr, CalledPartyNew}),
				SccpDec#sccp_msg{parameters=ParamsOut};
			    _ ->
				SccpDec
			end;
		    _ ->
			SccpDec
		end;
	    _ ->
		SccpDec
	end;
mangle_tt_sri_sm(_, _, _, SccpIn) ->
	SccpIn.
