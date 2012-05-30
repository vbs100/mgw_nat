% Eunit test rig for mangle_tt_sri_sm

-module(mangle_tt_sri_sm_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").

-include_lib("osmo_map/include/map.hrl").
-include_lib("osmo_ss7/include/isup.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/osmo_util.hrl").

-define(SCCP_MAP_INV_SRI_SM,
	#sccp_msg{msg_type = 9,
		  parameters = [{protocol_class, {0,0}},
			        {called_party_addr, #sccp_addr{res_nat_use = 0,
							       route_on_ssn = 0,
							       point_code = undefined,
							       ssn = 6,
							       global_title = #global_title{gti = 4,
											    nature_of_addr_ind = 4,
											    trans_type = 0,
											    encoding = undefined,
											    numbering_plan = 1,
											    phone_number = [9,1,3,0,0,0,0,0,0,0,1]}}},
				{calling_party_addr, #sccp_addr{res_nat_use = 0,
								route_on_ssn = 0,
								point_code = undefined,
								ssn = 8,
								global_title = #global_title{gti = 4,
											     nature_of_addr_ind = 4,
											     trans_type = 0,
											     encoding = undefined,
											     numbering_plan = 1,
											     phone_number = [9,8,7,0,0,0,0,0,0,1]}}},
				{user_data,<<98,70,72,4,81,1,13,65,107,30,40,28,6,7,0,17,134,5,1,1,1,160,17,96,15,128,2,7,128,161,9,6,7,4,0,0,1,0,20,2,108,30,161,28,2,1,64,2,1,45,48,20,128,7,145,114,39,67,83,32,249,129,1,1,130,6,145,83,132,9,0,103>>}]
		}).

-define(MAP_INV_SRI_SM, {'begin',
		#'MapSpecificPDUs_begin'{
		   otid = [81,1,2,200],
		   dialoguePortion = {'EXTERNAL', {syntax,{0,0,17,773,1,1,1}}, asn1_NOVALUE,
				      [96,15,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3]},
		   components = [{basicROS,
		     {invoke, #'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{
		       invokeId = {present,64},
		       linkedId = asn1_NOVALUE,
		       opcode = {local,45},
		       argument = #'RoutingInfoForSM-Arg'{
			 msisdn = [145,114,39,67,83,32,249],
			 'sm-RP-PRI' = true,
			 serviceCentreAddress = [145,83,132,9,0,103],
			 _ = asn1_NOVALUE},
		       _ = asn1_NOVALUE}}}],
		    _ = asn1_NOVALUE}}).

% helper functions

make_party_number(Digits) when is_integer(Digits) ->
	#party_number{phone_number = osmo_util:int2digit_list(Digits)}.

make_gt(Digits) when is_integer(Digits) ->
	#global_title{phone_number = osmo_util:int2digit_list(Digits)}.

make_sccp_sri_sm_to(CalledPartyNum) ->
	NumL = osmo_util:int2digit_list(CalledPartyNum),
	SccpIn = ?SCCP_MAP_INV_SRI_SM,
	Called = proplists:get_value(called_party_addr, SccpIn#sccp_msg.parameters),
	Dgt = Called#sccp_addr.global_title,
	CalledNew = Called#sccp_addr{global_title=Dgt#global_title{phone_number=NumL}},
	ParamsOut = lists:keyreplace(called_party_addr, 1, SccpIn#sccp_msg.parameters,
				     {called_party_addr, CalledNew}),
	SccpIn#sccp_msg{parameters=ParamsOut}.

get_dgt_tt(Sccp) when is_record(Sccp, sccp_msg) ->
	Called = proplists:get_value(called_party_addr, Sccp#sccp_msg.parameters),
	Dgt = Called#sccp_addr.global_title,
	Dgt#global_title.trans_type.


% actual test cases

tcap_comps() ->
	{'begin', BeginInvoke} = ?MAP_INV_SRI_SM,
	Comps = mangle_tt_sri_sm:get_tcap_components(?MAP_INV_SRI_SM),
	?assertEqual(BeginInvoke#'MapSpecificPDUs_begin'.components, Comps).

tcap_ops() ->
	Ops = mangle_tt_sri_sm:get_tcap_operations(?MAP_INV_SRI_SM),
	?assertEqual([{invoke,{local,45}}], Ops).

sri_sm() ->
	% test with decoded MAP as well as SCCP input
	?assertEqual(true, mangle_tt_sri_sm:check_for_invoke_sri_sm(?MAP_INV_SRI_SM)),
	?assertEqual(true, mangle_tt_sri_sm:check_for_invoke_sri_sm(?SCCP_MAP_INV_SRI_SM)).

isup_pfx_match() ->
	TrueNum = make_party_number(9101234567),
	FalseNum = make_party_number(4901234567),
	% test with integer and list input
	?assertEqual(true, mangle_tt_sri_sm:isup_party_match_pfx(TrueNum, 91)),
	?assertEqual(true, mangle_tt_sri_sm:isup_party_match_pfx(TrueNum, [9,1])),
	?assertEqual(false, mangle_tt_sri_sm:isup_party_match_pfx(FalseNum, 91)),
	?assertEqual(false, mangle_tt_sri_sm:isup_party_match_pfx(FalseNum, [9,1])).

gt_pfx_match() ->
	TrueNum = make_gt(9101234567),
	FalseNum = make_gt(4901234567),
	% test with integer and list input
	?assertEqual(true, mangle_tt_sri_sm:gt_match_pfx(TrueNum, 91)),
	?assertEqual(true, mangle_tt_sri_sm:gt_match_pfx(TrueNum, [9,1])),
	?assertEqual(false, mangle_tt_sri_sm:gt_match_pfx(FalseNum, 91)),
	?assertEqual(false, mangle_tt_sri_sm:gt_match_pfx(FalseNum, [9,1])).

gt_pfx_list_match() ->
	TrueNum = make_gt(9101234567),
	FalseNum = make_gt(4901234567),
	?assertEqual(true, mangle_tt_sri_sm:gt_match_pfx_list(TrueNum, [91, 53])),
	?assertEqual(true, mangle_tt_sri_sm:gt_match_pfx_list(TrueNum, [53, 91])),
	?assertEqual(false, mangle_tt_sri_sm:gt_match_pfx_list(FalseNum, [91, 53])),
	?assertEqual(false, mangle_tt_sri_sm:gt_match_pfx_list(FalseNum, [53, 91])).

tt_mangle() ->
	% test the overall macro-function for mangling the TT in case the DGT matches a
	% prefix and the message contains an Invoke(SRI-for-SM)
	Sccp91 = make_sccp_sri_sm_to(9101234567),
	SccpOut91 = mangle_tt_sri_sm:mangle_tt_sri_sm(from_msc, path, ?SCCP_MSGT_UDT, Sccp91),
	?assertEqual(3, get_dgt_tt(SccpOut91)),
	Sccp43 = make_sccp_sri_sm_to(4301234567),
	SccpOut43 = mangle_tt_sri_sm:mangle_tt_sri_sm(from_msc, path, ?SCCP_MSGT_UDT, Sccp91),
	?assertEqual(3, get_dgt_tt(SccpOut43)),
	Sccp49 = make_sccp_sri_sm_to(4901234567),
	SccpOut49 = mangle_tt_sri_sm:mangle_tt_sri_sm(from_msc, path, ?SCCP_MSGT_UDT, Sccp49),
	?assertEqual(get_dgt_tt(Sccp49), get_dgt_tt(SccpOut49)).


% setup and teardown

setup() ->
	application:set_env(mgw_nat, mangle_tt_sri_sm_pfx, [ 91, 43 ]).

teardown(_) ->
	application:unset_env(mgw_nat, mangle_tt_sri_sm_pfx).

mangle_tt_sri_test_() ->
	{setup,
		fun setup/0,
		fun teardown/1,
		[
			?_test(tcap_comps()),
			?_test(tcap_ops()),
			?_test(sri_sm()),
			?_test(isup_pfx_match()),
			?_test(gt_pfx_match()),
			?_test(gt_pfx_list_match()),
			?_test(tt_mangle())
		]
	}.
