% Eunit test rig for mangle_callbarr

-module(mangle_callbarr_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").

-include_lib("osmo_map/include/map.hrl").
-include_lib("osmo_ss7/include/osmo_util.hrl").

-define(SCCP_MAP_INS_SUB,
	#sccp_msg{msg_type = 9,
		  parameters = [{protocol_class, {0,0}},
				{called_party_addr, #sccp_addr{res_nat_use = 0,
							       route_on_ssn = 0,
							       point_code = undefined,
							       ssn = 7,
							       global_title = #global_title{gti = 4,
											    nature_of_addr_ind = 4,
											    trans_type = 0,
											    encoding = undefined,
											    numbering_plan = 1,
											    phone_number = [9,1,3,0,0,0,0,0,0,0,1]}}},
				{calling_party_addr, #sccp_addr{res_nat_use = 0,
								route_on_ssn = 0,
								point_code = undefined,
								ssn = 6,
								global_title = #global_title{gti = 4,
											     nature_of_addr_ind = 4,
											     trans_type = 0,
											     encoding = undefined,
											     numbering_plan = 1,
											     phone_number = [9,8,7,0,0,0,0,0,0,1]}}},
				{user_data,<<101,129,227,72,4,81,241,13,216,73,4,201,19,149,216,107,42,40,40,6,7,0,17,134,5,1,1,1,160,29,97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,163,5,161,3,2,1,0,108,129,168,161,129,165,2,1,65,2,1,7,48,129,156,129,7,145,83,52,88,0,16,86,130,1,10,131,1,1,164,66,4,1,17,4,1,18,4,1,19,4,1,48,4,1,64,4,1,33,4,1,34,4,1,35,4,1,36,4,1,37,4,1,38,4,1,39,4,1,28,4,1,29,4,1,30,4,1,31,4,1,56,4,1,72,4,1,44,4,1,45,4,1,46,4,1,47,166,12,4,1,17,4,1,18,4,1,33,4,1,34,167,50,161,37,4,1,146,48,32,48,6,130,1,24,132,1,5,48,6,130,1,16,132,1,5,48,6,131,1,32,132,1,5,48,6,131,1,16,132,1,5,163,9,4,1,17,132,1,4,129,1,1,168,5,3,3,1,0,0>>}]
		}).

% InsertSubscriberDataArg with provisionedSS but not for teleservice 20 (colr)
-define(MAP_INS_SUB_IN_NO20,
{continue,
 #'MapSpecificPDUs_continue'{
   otid = "Qñ\rØ",
   dtid = [201,19,149,216],
   dialoguePortion = #'EXTERNAL'{
	    'direct-reference' = {0,0,17,773,1,1,1},
	    'indirect-reference' = asn1_NOVALUE,
	    'data-value-descriptor' = asn1_NOVALUE,
	    encoding = {'single-ASN1-type',
		<<97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,163,
		  5,161,3,2,1,0>>}},
   components = [{basicROS,
	     {invoke,
	      #'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{
		     invokeId = {present,65},
		     linkedId = asn1_NOVALUE,
		     opcode = {local,7},
		     argument = #'InsertSubscriberDataArg'{
				imsi = asn1_NOVALUE,
				msisdn = [145,83,52,88,0,16,86],
				category = "\n",
				subscriberStatus = operatorDeterminedBarring,
			 	bearerServiceList = [[17], [18], [19], "0","@","!","\"","#","$","%","&","'", [28], [29], [30], [31], "8","H",",","-",".","/"],
			 	teleserviceList = [[17],[18],"!","\""],
				provisionedSS = [
			  		{'ss-Data', 
					 #'Ext-SS-Data'{
						'ss-Code' = [17],
						'ss-Status' = [4],
				  		'ss-SubscriptionOption' = {overrideCategory,overrideDisabled},
						basicServiceGroupList = asn1_NOVALUE,
						extensionContainer = asn1_NOVALUE}
					}],
				'odb-Data' = #'ODB-Data'{
				   'odb-GeneralData' = [], _ = asn1_NOVALUE}}}}}]}}).

% InsertSubscriberDataArg with provisionedSS including teleservice 20 (colr)
-define(MAP_INS_SUB_IN_20,
{continue,
 #'MapSpecificPDUs_continue'{
   otid = "Qñ\rØ",
   dtid = [201,19,149,216],
   dialoguePortion = #'EXTERNAL'{
	    'direct-reference' = {0,0,17,773,1,1,1},
	    'indirect-reference' = asn1_NOVALUE,
	    'data-value-descriptor' = asn1_NOVALUE,
	    encoding = {'single-ASN1-type',
		<<97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,163,
		  5,161,3,2,1,0>>}},
   components = [{basicROS,
	     {invoke,
	      #'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{
		     invokeId = {present,65},
		     linkedId = asn1_NOVALUE,
		     opcode = {local,7},
		     argument = #'InsertSubscriberDataArg'{
				imsi = asn1_NOVALUE,
				msisdn = [145,83,52,88,0,16,86],
				category = "\n",
				subscriberStatus = operatorDeterminedBarring,
			 	bearerServiceList = [[17], [18], [19], "0","@","!","\"","#","$","%","&","'", [28], [29], [30], [31], "8","H",",","-",".","/"],
			 	teleserviceList = [[17],[18],"!","\""],
				provisionedSS = [
			  		{'ss-Data', 
					 #'Ext-SS-Data'{
						'ss-Code' = [20],
						'ss-Status' = [4],
				  		'ss-SubscriptionOption' = {overrideCategory,overrideDisabled},
						basicServiceGroupList = asn1_NOVALUE,
						extensionContainer = asn1_NOVALUE}
					}],
				'odb-Data' = #'ODB-Data'{
				   'odb-GeneralData' = [], _ = asn1_NOVALUE}}}}}]}}).

-define(MAP_INS_SUB_OUT_20,
{continue,
 #'MapSpecificPDUs_continue'{
   otid = "Qñ\rØ",
   dtid = [201,19,149,216],
   dialoguePortion = #'EXTERNAL'{
	    'direct-reference' = {0,0,17,773,1,1,1},
	    'indirect-reference' = asn1_NOVALUE,
	    'data-value-descriptor' = asn1_NOVALUE,
	    encoding = {'single-ASN1-type',
		<<97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,163,
		  5,161,3,2,1,0>>}},
   components = [{basicROS,
	     {invoke,
	      #'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{
		     invokeId = {present,65},
		     linkedId = asn1_NOVALUE,
		     opcode = {local,7},
		     argument = #'InsertSubscriberDataArg'{
				imsi = asn1_NOVALUE,
				msisdn = [145,83,52,88,0,16,86],
				category = "\n",
				subscriberStatus = operatorDeterminedBarring,
			 	bearerServiceList = [[17], [18], [19], "0","@","!","\"","#","$","%","&","'", [28], [29], [30], [31], "8","H",",","-",".","/"],
			 	teleserviceList = [[17],[18],"!","\""],
				provisionedSS = [
			  		{'ss-Data', 
					 #'Ext-SS-Data'{
						'ss-Code' = [20],
						'ss-Status' = [4],
				  		'ss-SubscriptionOption' = {overrideCategory,overrideDisabled},
						basicServiceGroupList = asn1_NOVALUE,
						extensionContainer = asn1_NOVALUE}
					},
					{callBarringInfo, 
					 #'Ext-CallBarInfo'{
					    'ss-Code' = [146],
					    callBarringFeatureList = [
						#'Ext-CallBarringFeature'{
							basicService = {'ext-BearerService',[23]},
							'ss-Status' = [5],
							extensionContainer = asn1_NOVALUE},
				   		#'Ext-CallBarringFeature'{
							basicService = {'ext-BearerService',[42]},
							'ss-Status' = [10],
							extensionContainer = asn1_NOVALUE}],
				  	    extensionContainer = asn1_NOVALUE}}],
				'odb-Data' = #'ODB-Data'{
				   'odb-GeneralData' = [], _ = asn1_NOVALUE}}}}}]}}).


% InsertSubscriberDataArg with provisionedSS including BAOC
-define(MAP_INS_SUB_IN_BAOC,
{continue,
 #'MapSpecificPDUs_continue'{
   otid = "Qñ\rØ",
   dtid = [201,19,149,216],
   dialoguePortion = #'EXTERNAL'{
	    'direct-reference' = {0,0,17,773,1,1,1},
	    'indirect-reference' = asn1_NOVALUE,
	    'data-value-descriptor' = asn1_NOVALUE,
	    encoding = {'single-ASN1-type',
		<<97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,163,
		  5,161,3,2,1,0>>}},
   components = [{basicROS,
	     {invoke,
	      #'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{
		     invokeId = {present,65},
		     linkedId = asn1_NOVALUE,
		     opcode = {local,7},
		     argument = #'InsertSubscriberDataArg'{
				imsi = asn1_NOVALUE,
				msisdn = [145,83,52,88,0,16,86],
				category = "\n",
				subscriberStatus = operatorDeterminedBarring,
			 	bearerServiceList = [[17], [18], [19], "0","@","!","\"","#","$","%","&","'", [28], [29], [30], [31], "8","H",",","-",".","/"],
			 	teleserviceList = [[17],[18],"!","\""],
				provisionedSS = [
					{callBarringInfo,
					 #'Ext-CallBarInfo'{
					    'ss-Code' = [146],
					    callBarringFeatureList = [
						#'Ext-CallBarringFeature'{
							basicService = {'ext-BearerService',[24]},
							'ss-Status' = [5],
							extensionContainer = asn1_NOVALUE},
				   		#'Ext-CallBarringFeature'{
							basicService = {'ext-BearerService',[16]},
							'ss-Status' = [5],
							extensionContainer = asn1_NOVALUE},
				   		#'Ext-CallBarringFeature'{
							basicService = {'ext-Teleservice'," "},
							'ss-Status' = [5],
							extensionContainer = asn1_NOVALUE},
						#'Ext-CallBarringFeature'{
							basicService = {'ext-Teleservice',[16]},
							'ss-Status' = [5],
							extensionContainer = asn1_NOVALUE}],
				  	    extensionContainer = asn1_NOVALUE}},
			  		{'ss-Data', 
					 #'Ext-SS-Data'{
						'ss-Code' = [17],
						'ss-Status' = [4],
				  		'ss-SubscriptionOption' = {overrideCategory,overrideDisabled},
						basicServiceGroupList = asn1_NOVALUE,
						extensionContainer = asn1_NOVALUE}
					}],
				'odb-Data' = #'ODB-Data'{
				   'odb-GeneralData' = [], _ = asn1_NOVALUE}}}}}]}}).



-define(MAP_INS_SUB_OUT_BAOC,
{continue,
 #'MapSpecificPDUs_continue'{
   otid = "Qñ\rØ",
   dtid = [201,19,149,216],
   dialoguePortion = #'EXTERNAL'{
	    'direct-reference' = {0,0,17,773,1,1,1},
	    'indirect-reference' = asn1_NOVALUE,
	    'data-value-descriptor' = asn1_NOVALUE,
	    encoding = {'single-ASN1-type',
		<<97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,163,
		  5,161,3,2,1,0>>}},
   components = [{basicROS,
	     {invoke,
	      #'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{
		     invokeId = {present,65},
		     linkedId = asn1_NOVALUE,
		     opcode = {local,7},
		     argument = #'InsertSubscriberDataArg'{
				imsi = asn1_NOVALUE,
				msisdn = [145,83,52,88,0,16,86],
				category = "\n",
				subscriberStatus = operatorDeterminedBarring,
			 	bearerServiceList = [[17], [18], [19], "0","@","!","\"","#","$","%","&","'", [28], [29], [30], [31], "8","H",",","-",".","/"],
			 	teleserviceList = [[17],[18],"!","\""],
				provisionedSS = [
					{callBarringInfo, 
					 #'Ext-CallBarInfo'{
					    'ss-Code' = [146],
					    callBarringFeatureList = [
						#'Ext-CallBarringFeature'{
							basicService = {'ext-BearerService',[23]},
							'ss-Status' = [5],
							extensionContainer = asn1_NOVALUE},
				   		#'Ext-CallBarringFeature'{
							basicService = {'ext-BearerService',[42]},
							'ss-Status' = [10],
							extensionContainer = asn1_NOVALUE}],
				  	    extensionContainer = asn1_NOVALUE}},
			  		{'ss-Data', 
					 #'Ext-SS-Data'{
						'ss-Code' = [17],
						'ss-Status' = [4],
				  		'ss-SubscriptionOption' = {overrideCategory,overrideDisabled},
						basicServiceGroupList = asn1_NOVALUE,
						extensionContainer = asn1_NOVALUE}
					}],
				'odb-Data' = #'ODB-Data'{
				   'odb-GeneralData' = [], _ = asn1_NOVALUE}}}}}]}}).


% helper functions

% actual test cases

isd_20() ->
	MapOut = osmo_util:tuple_walk(?MAP_INS_SUB_IN_20,
				      fun mangle_callbarr:callbarr_twalk_cb/3, []),
	?assertEqual(?MAP_INS_SUB_OUT_20, MapOut).

isd_no20() ->
	MapOut = osmo_util:tuple_walk(?MAP_INS_SUB_IN_NO20,
				      fun mangle_callbarr:callbarr_twalk_cb/3, []),
	?assertEqual(?MAP_INS_SUB_IN_NO20, MapOut).

isd_baoc() ->
	MapOut = osmo_util:tuple_walk(?MAP_INS_SUB_IN_BAOC,
				      fun mangle_callbarr:callbarr_twalk_cb/3, []),
	?assertEqual(?MAP_INS_SUB_OUT_BAOC, MapOut).



% setup and teardown

setup() ->
	application:set_env(mgw_nat, mangle_callbarr_list, [ {23, "PA"}, {42,"QR"} ]).

teardown(_) ->
	application:unset_env(mgw_nat, mangle_callbarr_list).

mangle_callbarr_test_() ->
	{setup,
		fun setup/0,
		fun teardown/1,
		[
			?_test(isd_20()),
			?_test(isd_no20()),
			?_test(isd_baoc())
		]
	}.
