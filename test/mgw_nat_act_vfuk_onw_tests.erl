-module(mgw_nat_act_vfuk_onw_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").

-include_lib("osmo_map/include/map.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/osmo_util.hrl").



-define(MAP_DEC_IN, {'begin',
		#'MapSpecificPDUs_begin'{
		   otid = [81,1,2,200],
		   dialoguePortion = {'EXTERNAL', {syntax,{0,0,17,773,1,1,1}}, asn1_NOVALUE,
				      [96,15,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3]},
		   components = [{basicROS,
		     {invoke, #'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{
		       invokeId = {present,64},
		       linkedId = asn1_NOVALUE,
		       opcode = {local,2},
		       argument = #'UpdateLocationArg'{
			 imsi = [50,20,149,112,8,100,119,248],
			 'msc-Number' = [145,83,132,9,0,7],
			 'vlr-Number' = [145,83,132,9,0,23],
			 'vlr-Capability' = #'VLR-Capability'{
				supportedCamelPhases = [phase1, phase2, phase3],
				_ = asn1_NOVALUE},
			  _ = asn1_NOVALUE},
			_ = asn1_NOVALUE}}}]
		}}).
-define(MAP_DEC_OUT, {'begin',
		#'MapSpecificPDUs_begin'{
		   otid = [81,1,2,200],
		   dialoguePortion = {'EXTERNAL', {syntax,{0,0,17,773,1,1,1}}, asn1_NOVALUE,
				      [96,15,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3]},
		   components = [{basicROS,
		     {invoke, #'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{
		       invokeId = {present,64},
		       linkedId = asn1_NOVALUE,
		       opcode = {local,2},
		       argument = #'UpdateLocationArg'{
			 imsi = [50,20,149,112,8,100,119,248],
			 'msc-Number' = [145,83,132,9,0,7],
			 'vlr-Number' = [145,83,132,9,0,23],
			 'vlr-Capability' = #'VLR-Capability'{
				supportedCamelPhases = [phase1],
				_ = asn1_NOVALUE},
			 _ = asn1_NOVALUE},
		      _ = asn1_NOVALUE}}}]
	}}).

setup() ->
	application:set_env(mgw_nat, camel_phase_patch_table, [
			% each element in this list is a tuple of two lists:
			%  first half of the tuple: property-list of #gtt_match field members
			%  second half: list of atoms for camel phase [ phase1, phase2, phase3 ]
			{ [ {gt_range_from,	443850000000000 },
			    {gt_range_to,	443859999999999 } ], [ phase1 ] }
	]),
	mgw_nat_act_vfuk_onw:reload_config().

teardown(_) ->
	application:unset_env(mgw_nat, camel_phase_patch_table).

% Test the tuple walker and camelph_twalk_cb() directly, as we don't have a
% SCCP header in front of the MAP message and thus we cannot call
% mangle_map_camel_phase() directly
camelphase_twalk() ->
	?assertEqual(?MAP_DEC_OUT, osmo_util:tuple_walk(?MAP_DEC_IN,
							fun mgw_nat_act_vfuk_onw:camelph_twalk_cb/3,
							[[phase1]])).

build_fake_sccp_msg(CalledDigList) ->
	Gt = #global_title{phone_number = CalledDigList},
	SccpAddr = #sccp_addr{global_title = Gt},
	#sccp_msg{parameters = [{called_party_addr, SccpAddr}]}.

% a full test testing the entire chain...
camelphase_full() ->
	% Set up a fake SCCP message with Called Addr and GT
	SccpDec = build_fake_sccp_msg([4,4,3,8,5,0,0,0,0,0,0,0,0,0,1]),
	% call the rewrite actor
	MapOut = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_msc, [SccpDec], 0, ?MAP_DEC_IN),
	?assertEqual(?MAP_DEC_OUT, MapOut).

camelphase_full_nomatch() ->
	% Set up a fake SCCP message with Called Addr and GT
	SccpDec = build_fake_sccp_msg([4,4,3,8,6,5,4,3,2,1,2,3,4,5,1]),
	% call the rewrite actor
	MapOut = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_msc, [SccpDec], 0, ?MAP_DEC_IN),
	?assertEqual(?MAP_DEC_IN, MapOut).

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
		  ?_test(camelphase_full()),
		  ?_test(camelphase_full_nomatch()),
		  { timeout, 5*60, ?_test(test_pcap("../priv/map.pcap")) } ]
	}.
