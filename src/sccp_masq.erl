% ITU-T Q.71x SCCP UDT stateful masquerading

% (C) 2011 by Harald Welte <laforge@gnumonks.org>
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

-module(sccp_masq).
-author('Harald Welte <laforge@gnumonks.org>').
-include_lib("osmo_ss7/include/sccp.hrl").

-export([sccp_masq_msg/3, init/0, reset/0, dump/0, lookup_masq_addr/2]).

-compile([export_all]).

-record(sccp_masq_rec, {
	  digits_in,	% list of GT digits
	  digits_out,	% list of GT digits
	  last_access	% timestamp of last usage
	}).

% alloc + insert a new masquerade state record in our tables
masq_alloc(DigitsOrig) ->
	{ok, Base} = application:get_env(mgw_nat, sccp_masq_gt_base),
	{ok, Max} = application:get_env(mgw_nat, sccp_masq_gt_max),
	masq_try_alloc(DigitsOrig, Base, Max, 0).
masq_try_alloc(_DigitsOrig, _Base, Max, Offset) when Offset > Max ->
	undef;
masq_try_alloc(DigitsOrig, Base, Max, Offset) ->
	Try = Base + Offset,
	TryBin = osmo_util:int2digit_list(Try),
	% try to first allocate the reverse mapping, i.e. where the new
	% masqueraded address is the unique criteria for table lookup
	EtsRet = ets:insert_new(sccp_masq_rev,
				#sccp_masq_rec{digits_in = TryBin,
					       digits_out = DigitsOrig}),
	case EtsRet of
		false ->
			masq_try_alloc(DigitsOrig, Base, Max, Offset+1);
		_ ->
			ets:insert(sccp_masq_orig,
				   #sccp_masq_rec{digits_in = DigitsOrig,
						  digits_out = TryBin}),
			Try
	end.

% lookup a masqerade state record
lookup_masq_addr(orig, GtDigits) ->
	case ets:lookup(sccp_masq_orig, GtDigits) of
		[#sccp_masq_rec{digits_out = DigitsOut}] ->
			DigitsOut;
		_ ->
			% we do not allocate entries right here
			undef
	end;
lookup_masq_addr(rev, GtDigits) ->
	case ets:lookup(sccp_masq_rev, GtDigits) of
		[#sccp_masq_rec{digits_out = DigitsOut}] ->
			DigitsOut;
		_ ->
			% we do not allocate entries in the reverse direction
			undef
	end.

lookup_or_alloc_masq_addr(Dir, GtDigits) ->
	case lookup_masq_addr(Dir, GtDigits) of
		undef ->
			% allocate a new masq GT
			masq_alloc(GtDigits);
		Addr ->
			Addr
	end.

% Masquerade the CALLING address in first STP(G-MSC) -> HLR/VLR/MSC dir
mangle_rx_calling(from_stp, Addr = #sccp_addr{global_title = GT}) ->
	GtOrig = GT#global_title.phone_number,
	GtReplace = lookup_or_alloc_masq_addr(orig, GtOrig),
	case GtReplace of
		undef ->
			io:format("SCCP MASQ: Unable to rewrite in original direction (out of GT addrs?)~n"),
			Addr;
		_ ->
			io:format("SCCP MASQ (STP->MSC) rewrite ~p->~p~n", [GtOrig, GtReplace]),
			GTout = GT#global_title{phone_number = GtReplace},
			Addr#sccp_addr{global_title = GTout}
	end;
mangle_rx_calling(_From, Addr) ->
	Addr.

mangle_rx_called(from_msc, Addr = #sccp_addr{global_title = GT}) ->
	GtOrig = GT#global_title.phone_number,
	GtReplace = lookup_masq_addr(rev, GtOrig),
	case GtReplace of
		undef ->
			io:format("SCCP MASQ: Unable to rewrite in original direction (unknown GT ~p)~n", [GT]),
			Addr;
		_ ->
			io:format("SCCP MASQ (MSC->STP) rewrite ~p->~p~n", [GtOrig, GtReplace]),
			GTout = GT#global_title{phone_number = GtReplace},
			Addr#sccp_addr{global_title = GTout}
	end;
mangle_rx_called(_From, Addr) ->
	Addr.


sccp_masq_msg(From, ?SCCP_MSGT_UDT, Msg = #sccp_msg{parameters = Opts}) ->
	CalledParty = proplists:get_value(called_party_addr, Opts),
	CalledPartyNew = mangle_rx_called(From, CalledParty),
	CallingParty = proplists:get_value(calling_party_addr, Opts),
	CallingPartyNew = mangle_rx_calling(From, CallingParty),
	Opts1 = lists:keyreplace(called_party_addr, 1, Opts,
				 {called_party_addr, CalledPartyNew}),
	Opts2 = lists:keyreplace(calling_party_addr, 1, Opts1,
				 {calling_party_addr, CallingPartyNew}),
	Msg#sccp_msg{parameters = Opts2};
sccp_masq_msg(_From, _MsgType, Msg) ->
	Msg.

init() ->
	ets:new(sccp_masq_orig, [ordered_set, public, named_table,
				 {keypos, #sccp_masq_rec.digits_in}]),
	ets:new(sccp_masq_rev, [ordered_set, public, named_table,
				{keypos, #sccp_masq_rec.digits_in}]),
	ok.

reset() ->
	io:format("SCCP MASQ: Deleting all MASQ state records~n"),
	ets:delete_all_objects(sccp_masq_orig),
	ets:delete_all_objects(sccp_masq_rev).

dump() ->
	ListOrig = ets:tab2list(sccp_masq_orig),
	ListRev = ets:tab2list(sccp_masq_rev),
	io:format("SCCP MASQ Table Dump (ORIGINAL)~n"),
	dump_list(ListOrig),
	io:format("SCCP MASQ Table Dump (REVERSE)~n"),
	dump_list(ListRev).

dump_list([]) ->
	ok;
dump_list([#sccp_masq_rec{digits_in=DigIn, digits_out=DigOut}|Tail]) ->
	DigInInt = osmo_util:digit_list2int(DigIn),
	DigOutInt = osmo_util:digit_list2int(DigOut),
	io:format("~p -> ~p~n", [DigInInt, DigOutInt]),
	dump_list(Tail).
