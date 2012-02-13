-module(map_masq_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("osmo_map/include/map.hrl").

-define(OLD_PFX, [2,6,2,7,7]).
-define(NEW_PFX, [9,0,1,7,7]).

-define(IMSI_TRUE, [0,0,0,0,0,0,0,0,0,9]).
-define(IMSI_FALSE, [1,0,0,0,0,0,0,0,0,1]).
-define(SRI_SM_MATCH_IN, gen_sri_sm(?OLD_PFX ++ ?IMSI_TRUE)).
-define(SRI_SM_MATCH_OUT, gen_sri_sm(?NEW_PFX ++ ?IMSI_TRUE)).
-define(SRI_SM_NOMATCH_IN, gen_sri_sm(?OLD_PFX ++ ?IMSI_FALSE)).

gen_sri_sm(Imsi) when is_list(Imsi) ->
	{'begin', #'MapSpecificPDUs_begin'{otid=[0,0,0,1], components=[{basicROS, {returnResult, #'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult'{invokeId = 1, result=#'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result'{opcode={local, 2342}, result=#'RoutingInfoForSM-Res'{imsi=map_codec:encode_map_tbcd(Imsi), _ = asn1_NOVALUE}, _ = asn1_NOVALUE}, _ = asn1_NOVALUE}}}], _ = asn1_NOVALUE}}.


setup() ->
	Tree = imsi_list:read_list([?OLD_PFX ++ ?IMSI_TRUE]),
	application:set_env(mgw_nat, imsi_rewrite_tree, {Tree, ?OLD_PFX, ?NEW_PFX}).

teardown(_) ->
	application:unset_env(mgw_nat, imsi_rewrite_tree).

sri_sm_match() ->
	?assertEqual(?SRI_SM_MATCH_OUT, map_masq:mangle_map(from_msc, ?SRI_SM_MATCH_IN)).

sri_sm_nomatch() ->
	?assertEqual(?SRI_SM_NOMATCH_IN, map_masq:mangle_map(from_msc, ?SRI_SM_NOMATCH_IN)).

sri_sm_stp_msc() ->
	?assertEqual(?SRI_SM_MATCH_IN, map_masq:mangle_map(from_stp, ?SRI_SM_MATCH_IN)).

map_masq_test_() ->
	{setup,
		fun setup/0,
		fun teardown/1,
		[ ?_test(sri_sm_match()),
		  ?_test(sri_sm_nomatch()),
		  ?_test(sri_sm_stp_msc()) ]
	}.
