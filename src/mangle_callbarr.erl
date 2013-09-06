% Mangle the callBerringInfo inside provisionedSS of InsertSubscriberData

% (C) 2013 by Harald Welte <laforge@gnumonks.org>
% (C) 2013 OnWaves
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

-module(mangle_callbarr).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("osmo_map/include/map.hrl").

-export([get_ss_bit/1, get_ss_bits/1, bs_cfglist_to_ext_cbf/1, mangle_isd_repl_cbi/1,
	 mangle_pss_list/1, match_pss_colr/1]).

-export([callbarr_twalk_cb/3]).


get_ss_bit($Q) -> get_ss_bit(q);
get_ss_bit($P) -> get_ss_bit(p);
get_ss_bit($R) -> get_ss_bit(r);
get_ss_bit($A) -> get_ss_bit(a);
get_ss_bit($q) -> get_ss_bit(q);
get_ss_bit($p) -> get_ss_bit(p);
get_ss_bit($r) -> get_ss_bit(r);
get_ss_bit($a) -> get_ss_bit(a);
get_ss_bit(q) -> 8;
get_ss_bit(p) -> 4;
get_ss_bit(r) -> 2;
get_ss_bit(a) -> 1.

get_ss_bits(Int) when is_integer(Int) -> [Int];
get_ss_bits(Str) when is_list(Str) ->
	get_ss_bits(Str, 0).

get_ss_bits([], Int) ->
	[Int];
get_ss_bits([Head|Tail], Int) ->
	Bit = get_ss_bit(Head),
	get_ss_bits(Tail, Int bor Bit).

get_bs(Int) when is_integer(Int) ->
	[Int];
get_bs([Int]) when is_integer(Int) ->
	[Int].

bs_cfgtpl_to_ext_cbf({BS, Bits}) ->
	OutBS = get_bs(BS),
	OutBits = get_ss_bits(Bits),
	#'Ext-CallBarringFeature'{basicService = {'ext-BearerService', OutBS},
				  'ss-Status' = OutBits}.

% convert a list of tuples like [{23,"QP"},{24,"RA"}] into a list of
% 'Ext-CallBarringFeature'{} records
bs_cfglist_to_ext_cbf(List) when is_list(List) ->
	lists:map(fun bs_cfgtpl_to_ext_cbf/1, List).


% generate a List element for the provisionedSS list containing our constructed
% BAOC generated from the configuration file
gen_baoc() ->
	case application:get_env(mgw_nat, mangle_callbarr_list) of
		undefined ->
			[];
		{ok, PatchList} ->
			NL = bs_cfglist_to_ext_cbf(PatchList),
			[{callBarringInfo,
			  #'Ext-CallBarInfo'{'ss-Code' = ?'baoc',
					     callBarringFeatureList = NL}}]
	end.

% match on a provosionedSS of COLR
match_pss_colr({'ss-Data', #'Ext-SS-Data'{'ss-Code' = ?'colr'}}) -> true;
match_pss_colr(_) -> false.

% matcha and replace existing BAOC and replace it with our BAOC
match_repl_baoc(In = {callBarringInfo, #'Ext-CallBarInfo'{'ss-Code' = ?'baoc'}}) ->
	case gen_baoc() of
		[] -> In;
		[NewBaoc] -> NewBaoc
	end;
match_repl_baoc(In) -> In.


mangle_pss_list(asn1_NOVALUE) ->
	asn1_NOVALUE;
mangle_pss_list(List) when is_list(List) ->
	% 1) if there is already BAOC inside, replace BAOC
	List2 = lists:map(fun match_repl_baoc/1, List),
	% 2) if there is COLR in the provisionedSS list, append our
	% artificial BAOC element into the list
	case lists:any(fun match_pss_colr/1, List2) of
		true ->
			List2 ++ gen_baoc();
		false ->
			List2
	end.

mangle_isd_repl_cbi(ISD = #'InsertSubscriberDataArg'{provisionedSS=PSS}) ->
	ISD#'InsertSubscriberDataArg'{provisionedSS = mangle_pss_list(PSS)}.


% tuple walk callback for mgw_nat core
callbarr_twalk_cb(['continue', 'MapSpecificPDUs_continue', basicROS, invoke,
		   'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'],
		  ISD = #'InsertSubscriberDataArg'{}, _) ->
	mangle_isd_repl_cbi(ISD);
callbarr_twalk_cb(['begin', 'MapSpecificPDUs_begin', basicROS, invoke,
		   'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'],
		  ISD = #'InsertSubscriberDataArg'{}, _) ->
	mangle_isd_repl_cbi(ISD);
callbarr_twalk_cb(['end', 'MapSpecificPDUs_end', basicROS, invoke,
		   'MapSpecificPDUs_end_components_SEQOF_basicROS_invoke'],
		  ISD = #'InsertSubscriberDataArg'{}, _) ->
	mangle_isd_repl_cbi(ISD);
callbarr_twalk_cb(_Path, Msg, _Args) ->
	% Default case: simply return the unmodified tuple
	Msg.
