-module(mgw_nat_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").

-include_lib("osmo_map/include/map.hrl").
-include_lib("osmo_ss7/include/isup.hrl").

-define(PARTY_NUM_NAT, #party_number{phone_number = [9,1,1,4,8,5,3,2,1],
				     nature_of_addr_ind = ?ISUP_ADDR_NAT_NATIONAL}).

-define(PARTY_NUM_INT, #party_number{phone_number = [4,9,9,1,1,4,8,5,3,2,1],
				     nature_of_addr_ind = ?ISUP_ADDR_NAT_INTERNATIONAL}).
internationalize_test() ->
	?assertEqual(?PARTY_NUM_INT, mgw_nat:isup_party_internationalize(?PARTY_NUM_NAT, 49)).

nationalize_test() ->
	?assertEqual(?PARTY_NUM_NAT, mgw_nat:isup_party_nationalize(?PARTY_NUM_INT, 49)).

-define (PARTY_NUM_00_NAT, #party_number{phone_number = [0,0,4,9,9,1,1,4,8,5,3,2,1],
					 nature_of_addr_ind = ?ISUP_ADDR_NAT_NATIONAL}).

nat00_internationalize_test() ->
	?assertEqual(?PARTY_NUM_INT, mgw_nat:isup_party_nat00_internationalize(?PARTY_NUM_00_NAT)).
