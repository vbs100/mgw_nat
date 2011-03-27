-module(mgw_nat_act_vfuk_onw_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").

-include_lib("osmo_map/include/map.hrl").



-define(MAP_DEC_IN, {'begin',
                  {'MapSpecificPDUs_begin',
                   [81,1,2,200],
                   {'EXTERNAL',
                    {syntax,{0,0,17,773,1,1,1}},
                    asn1_NOVALUE,
                    [96,15,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3]},
                   [{basicROS,
                     {invoke,
                      {'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke',
                       {present,64},
                       asn1_NOVALUE,
                       {local,2},
                       {'UpdateLocationArg',
                        [50,20,149,112,8,100,119,248],
                        [145,83,132,9,0,7],
                        [145,83,132,9,0,23],
                        asn1_NOVALUE,asn1_NOVALUE,
                        {'VLR-Capability',
                         [phase1,phase2,phase3],
                         asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,
                         asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE},
                        asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,
                        asn1_NOVALUE}}}}]}}).
-define(MAP_DEC_OUT, {'begin',
                  {'MapSpecificPDUs_begin',
                   [81,1,2,200],
                   {'EXTERNAL',
                    {syntax,{0,0,17,773,1,1,1}},
                    asn1_NOVALUE,
                    [96,15,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3]},
                   [{basicROS,
                     {invoke,
                      {'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke',
                       {present,64},
                       asn1_NOVALUE,
                       {local,2},
                       {'UpdateLocationArg',
                        [50,20,149,112,8,100,119,248],
                        [145,83,132,9,0,7],
                        [145,83,132,9,0,23],
                        asn1_NOVALUE,asn1_NOVALUE,
                        {'VLR-Capability',
                         [phase1],
                         asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,
                         asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE},
                        asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,
                        asn1_NOVALUE}}}}]}}).

setup() ->
	application:set_env(mgw_nat, camel_phase_patch_table, [
                        % destination, phase-tuple-list
                        { 443859078046778, [phase1] }
                ]).

teardown(_) ->
	application:unset_env(undefined, camel_phase_patch_table).

% Test the tuple walker and camelph_twalk_cb() directly, as we don't have a
% SCCP header in front of the MAP message and thus we cannot call
% mangle_map_camel_phase() directly
camelphase_twalk() ->
	?assertEqual(?MAP_DEC_OUT, osmo_util:tuple_walk(?MAP_DEC_IN,
							fun mgw_nat_act_vfuk_onw:camelph_twalk_cb/3,
							[[phase1]])).

test_pcap(File) ->
	Args = [{rewrite_fn, fun mgw_nat_act_vfuk_onw:rewrite_actor/5}],
	case file:read_file_info(File) of
		{ok, _Info} ->
			{ok, NrPkts} = ?debugTime("PCAP", osmo_ss7_pcap:pcap_apply(File, "", Args)),
			?debugFmt("Parsed ~p PCAP packets~n", [NrPkts]);
		{error, _Reason} ->
			?debugFmt("Skipping PCAP based tests as no ~p could be found~n",
				  [File])
	end.

camel_phase_test_() ->
	{setup,
		fun setup/0,
		fun teardown/1,
		[ ?_test(camelphase_twalk()),
		  { timeout, 5*60, ?_test(test_pcap("../priv/map.pcap")) } ]
	}.

