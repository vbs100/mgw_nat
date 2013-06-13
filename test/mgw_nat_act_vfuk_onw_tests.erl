-module(mgw_nat_act_vfuk_onw_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").

-include_lib("osmo_map/include/map.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/osmo_util.hrl").

%%%%%%%%%%%%%% export functions so they can be called directly - remove later
-export([setup/0, teardown/1, csi_full_lu/0, csi_full_lu_nomatch/0, csi_full_isd1/0, csi_full_isd1a/0, csi_full_isd2/0, csi_full_isd3/0, csi_full_dsd1/0, csi_full_dsd2/0]).


-define(MAP_LU_DEC_IN, {'begin',
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
-define(MAP_LU_DEC_OUT, {'begin',
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

-define(MAP_LU2_DEC_IN,
{'begin',
    #'MapSpecificPDUs_begin'{
        otid = [81,241,15,85],
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',
                        <<96,15,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3>>}},
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,64},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,2},
                         argument = 
                             #'UpdateLocationArg'{
                                 imsi = [19,64,1,35,35,35,35,248],
                                 'msc-Number' = [145,83,35,35,35,7],
                                 'vlr-Number' = [145,83,35,35,35,23],
                                 lmsi = asn1_NOVALUE,extensionContainer = asn1_NOVALUE,
                                 'vlr-Capability' = 
                                     #'VLR-Capability'{
                                         supportedCamelPhases = [phase1,phase2],
                                         extensionContainer = asn1_NOVALUE,
                                         solsaSupportIndicator = asn1_NOVALUE,
                                         istSupportIndicator = asn1_NOVALUE,
                                         superChargerSupportedInServingNetworkEntity = asn1_NOVALUE,
                                         'longFTN-Supported' = asn1_NOVALUE,
                                         'supportedLCS-CapabilitySets' = asn1_NOVALUE,
                                         offeredCamel4CSIs = asn1_NOVALUE,
                                         'supportedRAT-TypesIndicator' = asn1_NOVALUE,
                                         'longGroupID-Supported' = asn1_NOVALUE},
                                 informPreviousNetworkEntity = asn1_NOVALUE,
                                 'cs-LCS-NotSupportedByUE' = asn1_NOVALUE,
                                 'v-gmlc-Address' = asn1_NOVALUE,'add-info' = asn1_NOVALUE,
                                 pagingArea = asn1_NOVALUE,
                                 skipSubscriberDataUpdate = asn1_NOVALUE,
                                 restorationIndicator = asn1_NOVALUE}}}}]}}).

-define(MAP_LU2_DEC_OUT,
{'begin',
    #'MapSpecificPDUs_begin'{
        otid = [81,241,15,85],
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',
                        <<96,15,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3>>}},
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,64},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,2},
                         argument = 
                             #'UpdateLocationArg'{
                                 imsi = [19,64,1,35,35,35,35,248],
                                 'msc-Number' = [145,83,35,35,35,7],
                                 'vlr-Number' = [145,83,35,35,35,23],
                                 lmsi = asn1_NOVALUE,extensionContainer = asn1_NOVALUE,
                                 'vlr-Capability' = 
                                     #'VLR-Capability'{
                                         supportedCamelPhases = [phase1,phase2,phase3],
                                         extensionContainer = asn1_NOVALUE,
                                         solsaSupportIndicator = asn1_NOVALUE,
                                         istSupportIndicator = asn1_NOVALUE,
                                         superChargerSupportedInServingNetworkEntity = asn1_NOVALUE,
                                         'longFTN-Supported' = asn1_NOVALUE,
                                         'supportedLCS-CapabilitySets' = asn1_NOVALUE,
                                         offeredCamel4CSIs = asn1_NOVALUE,
                                         'supportedRAT-TypesIndicator' = asn1_NOVALUE,
                                         'longGroupID-Supported' = asn1_NOVALUE},
                                 informPreviousNetworkEntity = asn1_NOVALUE,
                                 'cs-LCS-NotSupportedByUE' = asn1_NOVALUE,
                                 'v-gmlc-Address' = asn1_NOVALUE,'add-info' = asn1_NOVALUE,
                                 pagingArea = asn1_NOVALUE,
                                 skipSubscriberDataUpdate = asn1_NOVALUE,
                                 restorationIndicator = asn1_NOVALUE}}}}]}}).

-define(MAP_ISD1_DEC_IN,
{continue,
    #'MapSpecificPDUs_continue'{
        otid = [126,152,20,4],
        dtid = [81,241,15,85],
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',
                        <<97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,
                          163,5,161,3,2,1,0>>}},
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,63},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,7},
                         argument = 
                             #'InsertSubscriberDataArg'{
                                 imsi = asn1_NOVALUE,
                                 msisdn = [145,81,35,35,35,35,247],
                                 category = "\n",
                                 subscriberStatus = operatorDeterminedBarring,
                                 bearerServiceList = asn1_NOVALUE,
                                 teleserviceList = [[17],"!","\""],
                                 provisionedSS = 
                                     [{forwardingInfo,
                                          #'Ext-ForwInfo'{
                                              'ss-Code' = "!",
                                              forwardingFeatureList = 
                                                  [#'Ext-ForwFeature'{
                                                       basicService = {'ext-Teleservice',[16]},
                                                       'ss-Status' = "\f",forwardedToNumber = asn1_NOVALUE,
                                                       forwardedToSubaddress = asn1_NOVALUE,
                                                       forwardingOptions = asn1_NOVALUE,
                                                       noReplyConditionTime = asn1_NOVALUE,
                                                       extensionContainer = asn1_NOVALUE,
                                                       longForwardedToNumber = asn1_NOVALUE}],
                                              extensionContainer = asn1_NOVALUE}},
                                      {'ss-Data',
                                          #'Ext-SS-Data'{
                                              'ss-Code' = "B",
                                              'ss-Status' = [5],
                                              'ss-SubscriptionOption' = asn1_NOVALUE,
                                              basicServiceGroupList = [{'ext-Teleservice',[16]}],
                                              extensionContainer = asn1_NOVALUE}}],
                                 'odb-Data' = asn1_NOVALUE,
                                 roamingRestrictionDueToUnsupportedFeature = asn1_NOVALUE,
                                 regionalSubscriptionData = asn1_NOVALUE,
                                 vbsSubscriptionData = asn1_NOVALUE,
                                 vgcsSubscriptionData = asn1_NOVALUE,
                                 vlrCamelSubscriptionInfo = asn1_NOVALUE,
                                 extensionContainer = asn1_NOVALUE,
                                 'naea-PreferredCI' = asn1_NOVALUE,
                                 gprsSubscriptionData = asn1_NOVALUE,
                                 roamingRestrictedInSgsnDueToUnsupportedFeature = 
                                     asn1_NOVALUE,
                                 networkAccessMode = asn1_NOVALUE,
                                 lsaInformation = asn1_NOVALUE,
                                 'lmu-Indicator' = asn1_NOVALUE,
                                 lcsInformation = asn1_NOVALUE,istAlertTimer = asn1_NOVALUE,
                                 superChargerSupportedInHLR = asn1_NOVALUE,
                                 'mc-SS-Info' = asn1_NOVALUE,
                                 'cs-AllocationRetentionPriority' = asn1_NOVALUE,
                                 'sgsn-CAMEL-SubscriptionInfo' = asn1_NOVALUE,
                                 chargingCharacteristics = asn1_NOVALUE,
                                 accessRestrictionData = asn1_NOVALUE,
                                 'ics-Indicator' = asn1_NOVALUE,
                                 'eps-SubscriptionData' = asn1_NOVALUE,
                                 'csg-SubscriptionDataList' = asn1_NOVALUE,
                                 'ue-ReachabilityRequestIndicator' = asn1_NOVALUE,
                                 'sgsn-Number' = asn1_NOVALUE,
                                 'mme-Name' = asn1_NOVALUE}}}}]}}).

-define(MAP_ISD1A_DEC_IN,
{continue,
    #'MapSpecificPDUs_continue'{
        otid = [126,152,20,4],
        dtid = [81,241,15,85],
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',
                        <<97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,
                          163,5,161,3,2,1,0>>}},
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,63},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,7},
                         argument = 
                             #'InsertSubscriberDataArg'{
                                 imsi = asn1_NOVALUE,
                                 msisdn = asn1_NOVALUE,
                                 category = "\n",
                                 subscriberStatus = operatorDeterminedBarring,
                                 bearerServiceList = asn1_NOVALUE,
                                 teleserviceList = [[17],"!","\""],
                                 provisionedSS = 
                                     [{forwardingInfo,
                                          #'Ext-ForwInfo'{
                                              'ss-Code' = "!",
                                              forwardingFeatureList = 
                                                  [#'Ext-ForwFeature'{
                                                       basicService = {'ext-Teleservice',[16]},
                                                       'ss-Status' = "\f",forwardedToNumber = asn1_NOVALUE,
                                                       forwardedToSubaddress = asn1_NOVALUE,
                                                       forwardingOptions = asn1_NOVALUE,
                                                       noReplyConditionTime = asn1_NOVALUE,
                                                       extensionContainer = asn1_NOVALUE,
                                                       longForwardedToNumber = asn1_NOVALUE}],
                                              extensionContainer = asn1_NOVALUE}},
                                      {'ss-Data',
                                          #'Ext-SS-Data'{
                                              'ss-Code' = "B",
                                              'ss-Status' = [5],
                                              'ss-SubscriptionOption' = asn1_NOVALUE,
                                              basicServiceGroupList = [{'ext-Teleservice',[16]}],
                                              extensionContainer = asn1_NOVALUE}}],
                                 'odb-Data' = asn1_NOVALUE,
                                 roamingRestrictionDueToUnsupportedFeature = asn1_NOVALUE,
                                 regionalSubscriptionData = asn1_NOVALUE,
                                 vbsSubscriptionData = asn1_NOVALUE,
                                 vgcsSubscriptionData = asn1_NOVALUE,
                                 vlrCamelSubscriptionInfo =
                                    #'VlrCamelSubscriptionInfo'{
                                         'o-CSI' = 
                                             #'O-CSI'{
                                                 'o-BcsmCamelTDPDataList' = 
                                                     [#'O-BcsmCamelTDPData'{
                                                          'o-BcsmTriggerDetectionPoint' = collectedInfo,
                                                          serviceKey = 304,
                                                          'gsmSCF-Address' = [145,145,35,35,35,35,242],
                                                          defaultCallHandling = continueCall,
                                                          extensionContainer = asn1_NOVALUE}],
                                                 extensionContainer = asn1_NOVALUE,
                                                 camelCapabilityHandling = 1,
                                                 notificationToCSE = asn1_NOVALUE,csiActive = asn1_NOVALUE},
                                         extensionContainer = asn1_NOVALUE,'ss-CSI' = asn1_NOVALUE,
                                         'o-BcsmCamelTDP-CriteriaList' = 
                                             [#'O-BcsmCamelTDP-Criteria'{
                                                  'o-BcsmTriggerDetectionPoint' = collectedInfo,
                                                  destinationNumberCriteria = asn1_NOVALUE,
                                                  basicServiceCriteria = asn1_NOVALUE,
                                                  callTypeCriteria = asn1_NOVALUE,
                                                  'o-CauseValueCriteria' = asn1_NOVALUE,
                                                  extensionContainer = asn1_NOVALUE}],
                                         'tif-CSI' = asn1_NOVALUE,'m-CSI' = asn1_NOVALUE,
					 'mo-sms-CSI' = asn1_NOVALUE,
					 'vt-CSI' = asn1_NOVALUE,
                                         't-BCSM-CAMEL-TDP-CriteriaList' = asn1_NOVALUE,
                                         'd-CSI' = asn1_NOVALUE,'mt-sms-CSI' = asn1_NOVALUE,
                                         'mt-smsCAMELTDP-CriteriaList' = asn1_NOVALUE},
                                 extensionContainer = asn1_NOVALUE,
                                 'naea-PreferredCI' = asn1_NOVALUE,
                                 gprsSubscriptionData = asn1_NOVALUE,
                                 roamingRestrictedInSgsnDueToUnsupportedFeature = 
                                     asn1_NOVALUE,
                                 networkAccessMode = asn1_NOVALUE,
                                 lsaInformation = asn1_NOVALUE,
                                 'lmu-Indicator' = asn1_NOVALUE,
                                 lcsInformation = asn1_NOVALUE,istAlertTimer = asn1_NOVALUE,
                                 superChargerSupportedInHLR = asn1_NOVALUE,
                                 'mc-SS-Info' = asn1_NOVALUE,
                                 'cs-AllocationRetentionPriority' = asn1_NOVALUE,
                                 'sgsn-CAMEL-SubscriptionInfo' = asn1_NOVALUE,
                                 chargingCharacteristics = asn1_NOVALUE,
                                 accessRestrictionData = asn1_NOVALUE,
                                 'ics-Indicator' = asn1_NOVALUE,
                                 'eps-SubscriptionData' = asn1_NOVALUE,
                                 'csg-SubscriptionDataList' = asn1_NOVALUE,
                                 'ue-ReachabilityRequestIndicator' = asn1_NOVALUE,
                                 'sgsn-Number' = asn1_NOVALUE,
                                 'mme-Name' = asn1_NOVALUE}}}}]}}).

-define(MAP_ISD1A_DEC_OUT,
{continue,
    #'MapSpecificPDUs_continue'{
        otid = [126,152,20,4],
        dtid = [81,241,15,85],
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',
                        <<97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,
                          163,5,161,3,2,1,0>>}},
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,63},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,7},
                         argument = 
                             #'InsertSubscriberDataArg'{
                                 imsi = asn1_NOVALUE,
                                 msisdn = asn1_NOVALUE,
                                 category = "\n",
                                 subscriberStatus = operatorDeterminedBarring,
                                 bearerServiceList = asn1_NOVALUE,
                                 teleserviceList = [[17],"!","\""],
                                 provisionedSS = 
                                     [{forwardingInfo,
                                          #'Ext-ForwInfo'{
                                              'ss-Code' = "!",
                                              forwardingFeatureList = 
                                                  [#'Ext-ForwFeature'{
                                                       basicService = {'ext-Teleservice',[16]},
                                                       'ss-Status' = "\f",forwardedToNumber = asn1_NOVALUE,
                                                       forwardedToSubaddress = asn1_NOVALUE,
                                                       forwardingOptions = asn1_NOVALUE,
                                                       noReplyConditionTime = asn1_NOVALUE,
                                                       extensionContainer = asn1_NOVALUE,
                                                       longForwardedToNumber = asn1_NOVALUE}],
                                              extensionContainer = asn1_NOVALUE}},
                                      {'ss-Data',
                                          #'Ext-SS-Data'{
                                              'ss-Code' = "B",
                                              'ss-Status' = [5],
                                              'ss-SubscriptionOption' = asn1_NOVALUE,
                                              basicServiceGroupList = [{'ext-Teleservice',[16]}],
                                              extensionContainer = asn1_NOVALUE}}],
                                 'odb-Data' = asn1_NOVALUE,
                                 roamingRestrictionDueToUnsupportedFeature = asn1_NOVALUE,
                                 regionalSubscriptionData = asn1_NOVALUE,
                                 vbsSubscriptionData = asn1_NOVALUE,
                                 vgcsSubscriptionData = asn1_NOVALUE,
                                 vlrCamelSubscriptionInfo = asn1_NOVALUE,
                                 extensionContainer = asn1_NOVALUE,
                                 'naea-PreferredCI' = asn1_NOVALUE,
                                 gprsSubscriptionData = asn1_NOVALUE,
                                 roamingRestrictedInSgsnDueToUnsupportedFeature = 
                                     asn1_NOVALUE,
                                 networkAccessMode = asn1_NOVALUE,
                                 lsaInformation = asn1_NOVALUE,
                                 'lmu-Indicator' = asn1_NOVALUE,
                                 lcsInformation = asn1_NOVALUE,istAlertTimer = asn1_NOVALUE,
                                 superChargerSupportedInHLR = asn1_NOVALUE,
                                 'mc-SS-Info' = asn1_NOVALUE,
                                 'cs-AllocationRetentionPriority' = asn1_NOVALUE,
                                 'sgsn-CAMEL-SubscriptionInfo' = asn1_NOVALUE,
                                 chargingCharacteristics = asn1_NOVALUE,
                                 accessRestrictionData = asn1_NOVALUE,
                                 'ics-Indicator' = asn1_NOVALUE,
                                 'eps-SubscriptionData' = asn1_NOVALUE,
                                 'csg-SubscriptionDataList' = asn1_NOVALUE,
                                 'ue-ReachabilityRequestIndicator' = asn1_NOVALUE,
                                 'sgsn-Number' = asn1_NOVALUE,
                                 'mme-Name' = asn1_NOVALUE}}}}]}}).

-define(MAP_ISD2_DEC_IN,
{'begin',
    #'MapSpecificPDUs_begin'{
        otid = "Ói«Æ",
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',
                        <<96,15,128,2,7,128,161,9,6,7,4,0,0,1,0,16,3>>}},
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,1},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,7},
                         argument = 
                             #'InsertSubscriberDataArg'{
                                 imsi = [19,64,1,35,35,35,35,248],
                                 msisdn = asn1_NOVALUE,category = asn1_NOVALUE,
                                 subscriberStatus = asn1_NOVALUE,
                                 bearerServiceList = asn1_NOVALUE,
                                 teleserviceList = asn1_NOVALUE,provisionedSS = asn1_NOVALUE,
                                 'odb-Data' = asn1_NOVALUE,
                                 roamingRestrictionDueToUnsupportedFeature = asn1_NOVALUE,
                                 regionalSubscriptionData = asn1_NOVALUE,
                                 vbsSubscriptionData = asn1_NOVALUE,
                                 vgcsSubscriptionData = asn1_NOVALUE,
                                 vlrCamelSubscriptionInfo = 
                                     #'VlrCamelSubscriptionInfo'{
                                         'o-CSI' = 
                                             #'O-CSI'{
                                                 'o-BcsmCamelTDPDataList' = 
                                                     [#'O-BcsmCamelTDPData'{
                                                          'o-BcsmTriggerDetectionPoint' = collectedInfo,
                                                          serviceKey = 304,
                                                          'gsmSCF-Address' = [145,145,35,35,35,35,242],
                                                          defaultCallHandling = continueCall,
                                                          extensionContainer = asn1_NOVALUE}],
                                                 extensionContainer = asn1_NOVALUE,
                                                 camelCapabilityHandling = 1,
                                                 notificationToCSE = asn1_NOVALUE,csiActive = asn1_NOVALUE},
                                         extensionContainer = asn1_NOVALUE,'ss-CSI' = asn1_NOVALUE,
                                         'o-BcsmCamelTDP-CriteriaList' = 
                                             [#'O-BcsmCamelTDP-Criteria'{
                                                  'o-BcsmTriggerDetectionPoint' = collectedInfo,
                                                  destinationNumberCriteria = asn1_NOVALUE,
                                                  basicServiceCriteria = asn1_NOVALUE,
                                                  callTypeCriteria = asn1_NOVALUE,
                                                  'o-CauseValueCriteria' = asn1_NOVALUE,
                                                  extensionContainer = asn1_NOVALUE}],
                                         'tif-CSI' = asn1_NOVALUE,'m-CSI' = asn1_NOVALUE,
					'mo-sms-CSI' =
					    #'SMS-CSI'{
					       'sms-CAMEL-TDP-DataList' =
						   [#'SMS-CAMEL-TDP-Data'{
						       'sms-TriggerDetectionPoint' = 'sms-CollectedInfo',
						       serviceKey = 23,
						       'gsmSCF-Address' = [145,145,35,35,35,35,242],
						       'defaultSMS-Handling' = releaseTransaction,
						       extensionContainer = asn1_NOVALUE}],
					       camelCapabilityHandling = 3,
					       extensionContainer = asn1_NOVALUE,
					       notificationToCSE = asn1_NOVALUE,
					       'csi-Active' = asn1_NOVALUE},
					 'vt-CSI' = asn1_NOVALUE,
                                         't-BCSM-CAMEL-TDP-CriteriaList' = asn1_NOVALUE,
                                         'd-CSI' = asn1_NOVALUE,'mt-sms-CSI' = asn1_NOVALUE,
                                         'mt-smsCAMELTDP-CriteriaList' = asn1_NOVALUE},
                                 extensionContainer = asn1_NOVALUE,
                                 'naea-PreferredCI' = asn1_NOVALUE,
                                 gprsSubscriptionData = asn1_NOVALUE,
                                 roamingRestrictedInSgsnDueToUnsupportedFeature = 
                                     asn1_NOVALUE,
                                 networkAccessMode = asn1_NOVALUE,
                                 lsaInformation = asn1_NOVALUE,
                                 'lmu-Indicator' = asn1_NOVALUE,
                                 lcsInformation = asn1_NOVALUE,istAlertTimer = asn1_NOVALUE,
                                 superChargerSupportedInHLR = asn1_NOVALUE,
                                 'mc-SS-Info' = asn1_NOVALUE,
                                 'cs-AllocationRetentionPriority' = asn1_NOVALUE,
                                 'sgsn-CAMEL-SubscriptionInfo' = asn1_NOVALUE,
                                 chargingCharacteristics = asn1_NOVALUE,
                                 accessRestrictionData = asn1_NOVALUE,
                                 'ics-Indicator' = asn1_NOVALUE,
                                 'eps-SubscriptionData' = asn1_NOVALUE,
                                 'csg-SubscriptionDataList' = asn1_NOVALUE,
                                 'ue-ReachabilityRequestIndicator' = asn1_NOVALUE,
                                 'sgsn-Number' = asn1_NOVALUE,
                                 'mme-Name' = asn1_NOVALUE}}}}]}}).

-define(MAP_ISD2_DEC_OUT,
{'begin',
    #'MapSpecificPDUs_begin'{
        otid = "Ói«Æ",
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',
                        <<96,15,128,2,7,128,161,9,6,7,4,0,0,1,0,16,3>>}},
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,1},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,7},
                         argument = 
                             #'InsertSubscriberDataArg'{
                                 imsi = [19,64,1,35,35,35,35,248],
                                 msisdn = asn1_NOVALUE,category = asn1_NOVALUE,
                                 subscriberStatus = asn1_NOVALUE,
                                 bearerServiceList = asn1_NOVALUE,
                                 teleserviceList = asn1_NOVALUE,provisionedSS = asn1_NOVALUE,
                                 'odb-Data' = asn1_NOVALUE,
                                 roamingRestrictionDueToUnsupportedFeature = asn1_NOVALUE,
                                 regionalSubscriptionData = asn1_NOVALUE,
                                 vbsSubscriptionData = asn1_NOVALUE,
                                 vgcsSubscriptionData = asn1_NOVALUE,
                                 vlrCamelSubscriptionInfo = asn1_NOVALUE,
                                 extensionContainer = asn1_NOVALUE,
                                 'naea-PreferredCI' = asn1_NOVALUE,
                                 gprsSubscriptionData = asn1_NOVALUE,
                                 roamingRestrictedInSgsnDueToUnsupportedFeature = 
                                     asn1_NOVALUE,
                                 networkAccessMode = asn1_NOVALUE,
                                 lsaInformation = asn1_NOVALUE,
                                 'lmu-Indicator' = asn1_NOVALUE,
                                 lcsInformation = asn1_NOVALUE,istAlertTimer = asn1_NOVALUE,
                                 superChargerSupportedInHLR = asn1_NOVALUE,
                                 'mc-SS-Info' = asn1_NOVALUE,
                                 'cs-AllocationRetentionPriority' = asn1_NOVALUE,
                                 'sgsn-CAMEL-SubscriptionInfo' = asn1_NOVALUE,
                                 chargingCharacteristics = asn1_NOVALUE,
                                 accessRestrictionData = asn1_NOVALUE,
                                 'ics-Indicator' = asn1_NOVALUE,
                                 'eps-SubscriptionData' = asn1_NOVALUE,
                                 'csg-SubscriptionDataList' = asn1_NOVALUE,
                                 'ue-ReachabilityRequestIndicator' = asn1_NOVALUE,
                                 'sgsn-Number' = asn1_NOVALUE,
                                 'mme-Name' = asn1_NOVALUE}}}}]}}).

-define(MAP_ISD3_DEC_IN,
{continue,
    #'MapSpecificPDUs_continue'{
        otid = [126,152,20,4],
        dtid = [81,241,15,85],
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',
                        <<97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,
                          163,5,161,3,2,1,0>>}},
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,63},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,7},
                         argument = 
                             #'InsertSubscriberDataArg'{
                                 imsi = asn1_NOVALUE,
                                 msisdn = [145,81,35,35,35,35,247],
                                 category = "\n",
                                 subscriberStatus = operatorDeterminedBarring,
                                 bearerServiceList = asn1_NOVALUE,
                                 teleserviceList = [[17],"!","\""],
                                 provisionedSS = 
                                     [{forwardingInfo,
                                          #'Ext-ForwInfo'{
                                              'ss-Code' = "!",
                                              forwardingFeatureList = 
                                                  [#'Ext-ForwFeature'{
                                                       basicService = {'ext-Teleservice',[16]},
                                                       'ss-Status' = "\f",forwardedToNumber = asn1_NOVALUE,
                                                       forwardedToSubaddress = asn1_NOVALUE,
                                                       forwardingOptions = asn1_NOVALUE,
                                                       noReplyConditionTime = asn1_NOVALUE,
                                                       extensionContainer = asn1_NOVALUE,
                                                       longForwardedToNumber = asn1_NOVALUE}],
                                              extensionContainer = asn1_NOVALUE}},
                                      {'ss-Data',
                                          #'Ext-SS-Data'{
                                              'ss-Code' = "B",
                                              'ss-Status' = [5],
                                              'ss-SubscriptionOption' = asn1_NOVALUE,
                                              basicServiceGroupList = [{'ext-Teleservice',[16]}],
                                              extensionContainer = asn1_NOVALUE}}],
                                 'odb-Data' = asn1_NOVALUE,
                                 roamingRestrictionDueToUnsupportedFeature = asn1_NOVALUE,
                                 regionalSubscriptionData = asn1_NOVALUE,
                                 vbsSubscriptionData = asn1_NOVALUE,
                                 vgcsSubscriptionData = asn1_NOVALUE,
                                 vlrCamelSubscriptionInfo =
                                     #'VlrCamelSubscriptionInfo'{
                                         'o-CSI' = 
                                             #'O-CSI'{
                                                 'o-BcsmCamelTDPDataList' = 
                                                     [#'O-BcsmCamelTDPData'{
                                                          'o-BcsmTriggerDetectionPoint' = collectedInfo,
                                                          serviceKey = 304,
                                                          'gsmSCF-Address' = [145,145,35,35,35,35,242],
                                                          defaultCallHandling = continueCall,
                                                          extensionContainer = asn1_NOVALUE}],
                                                 extensionContainer = asn1_NOVALUE,
                                                 camelCapabilityHandling = 1,
                                                 notificationToCSE = asn1_NOVALUE,csiActive = asn1_NOVALUE},
                                         extensionContainer = asn1_NOVALUE,'ss-CSI' = asn1_NOVALUE,
                                         'o-BcsmCamelTDP-CriteriaList' = 
                                             [#'O-BcsmCamelTDP-Criteria'{
                                                  'o-BcsmTriggerDetectionPoint' = collectedInfo,
                                                  destinationNumberCriteria = asn1_NOVALUE,
                                                  basicServiceCriteria = asn1_NOVALUE,
                                                  callTypeCriteria = asn1_NOVALUE,
                                                  'o-CauseValueCriteria' = asn1_NOVALUE,
                                                  extensionContainer = asn1_NOVALUE}],
                                         'tif-CSI' = asn1_NOVALUE,'m-CSI' = asn1_NOVALUE,
					'mo-sms-CSI' =
					    #'SMS-CSI'{
					       'sms-CAMEL-TDP-DataList' =
						   [#'SMS-CAMEL-TDP-Data'{
						       'sms-TriggerDetectionPoint' = 'sms-CollectedInfo',
						       serviceKey = 23,
						       'gsmSCF-Address' = [145,145,35,35,35,35,242],
						       'defaultSMS-Handling' = releaseTransaction,
						       extensionContainer = asn1_NOVALUE}],
					       camelCapabilityHandling = 3,
					       extensionContainer = asn1_NOVALUE,
					       notificationToCSE = asn1_NOVALUE,
					       'csi-Active' = asn1_NOVALUE},
					 'vt-CSI' = asn1_NOVALUE,
                                         't-BCSM-CAMEL-TDP-CriteriaList' = asn1_NOVALUE,
                                         'd-CSI' = asn1_NOVALUE,'mt-sms-CSI' = asn1_NOVALUE,
                                         'mt-smsCAMELTDP-CriteriaList' = asn1_NOVALUE},
                                 extensionContainer = asn1_NOVALUE,
                                 'naea-PreferredCI' = asn1_NOVALUE,
                                 gprsSubscriptionData = asn1_NOVALUE,
                                 roamingRestrictedInSgsnDueToUnsupportedFeature = 
                                     asn1_NOVALUE,
                                 networkAccessMode = asn1_NOVALUE,
                                 lsaInformation = asn1_NOVALUE,
                                 'lmu-Indicator' = asn1_NOVALUE,
                                 lcsInformation = asn1_NOVALUE,istAlertTimer = asn1_NOVALUE,
                                 superChargerSupportedInHLR = asn1_NOVALUE,
                                 'mc-SS-Info' = asn1_NOVALUE,
                                 'cs-AllocationRetentionPriority' = asn1_NOVALUE,
                                 'sgsn-CAMEL-SubscriptionInfo' = asn1_NOVALUE,
                                 chargingCharacteristics = asn1_NOVALUE,
                                 accessRestrictionData = asn1_NOVALUE,
                                 'ics-Indicator' = asn1_NOVALUE,
                                 'eps-SubscriptionData' = asn1_NOVALUE,
                                 'csg-SubscriptionDataList' = asn1_NOVALUE,
                                 'ue-ReachabilityRequestIndicator' = asn1_NOVALUE,
                                 'sgsn-Number' = asn1_NOVALUE,
                                 'mme-Name' = asn1_NOVALUE}}}}]}}).

-define(MAP_ISD3_DEC_OUT,
{continue,
    #'MapSpecificPDUs_continue'{
        otid = [126,152,20,4],
        dtid = [81,241,15,85],
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',
                        <<97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,
                          163,5,161,3,2,1,0>>}},
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,63},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,7},
                         argument = 
                             #'InsertSubscriberDataArg'{
                                 imsi = asn1_NOVALUE,
                                 msisdn = [145,81,35,35,35,35,247],
                                 category = "\n",
                                 subscriberStatus = operatorDeterminedBarring,
                                 bearerServiceList = asn1_NOVALUE,
                                 teleserviceList = [[17],"!","\""],
                                 provisionedSS = 
                                     [{forwardingInfo,
                                          #'Ext-ForwInfo'{
                                              'ss-Code' = "!",
                                              forwardingFeatureList = 
                                                  [#'Ext-ForwFeature'{
                                                       basicService = {'ext-Teleservice',[16]},
                                                       'ss-Status' = "\f",forwardedToNumber = asn1_NOVALUE,
                                                       forwardedToSubaddress = asn1_NOVALUE,
                                                       forwardingOptions = asn1_NOVALUE,
                                                       noReplyConditionTime = asn1_NOVALUE,
                                                       extensionContainer = asn1_NOVALUE,
                                                       longForwardedToNumber = asn1_NOVALUE}],
                                              extensionContainer = asn1_NOVALUE}},
                                      {'ss-Data',
                                          #'Ext-SS-Data'{
                                              'ss-Code' = "B",
                                              'ss-Status' = [5],
                                              'ss-SubscriptionOption' = asn1_NOVALUE,
                                              basicServiceGroupList = [{'ext-Teleservice',[16]}],
                                              extensionContainer = asn1_NOVALUE}}],
                                 'odb-Data' = asn1_NOVALUE,
                                 roamingRestrictionDueToUnsupportedFeature = asn1_NOVALUE,
                                 regionalSubscriptionData = asn1_NOVALUE,
                                 vbsSubscriptionData = asn1_NOVALUE,
                                 vgcsSubscriptionData = asn1_NOVALUE,
                                 vlrCamelSubscriptionInfo = asn1_NOVALUE,
                                 extensionContainer = asn1_NOVALUE,
                                 'naea-PreferredCI' = asn1_NOVALUE,
                                 gprsSubscriptionData = asn1_NOVALUE,
                                 roamingRestrictedInSgsnDueToUnsupportedFeature = 
                                     asn1_NOVALUE,
                                 networkAccessMode = asn1_NOVALUE,
                                 lsaInformation = asn1_NOVALUE,
                                 'lmu-Indicator' = asn1_NOVALUE,
                                 lcsInformation = asn1_NOVALUE,istAlertTimer = asn1_NOVALUE,
                                 superChargerSupportedInHLR = asn1_NOVALUE,
                                 'mc-SS-Info' = asn1_NOVALUE,
                                 'cs-AllocationRetentionPriority' = asn1_NOVALUE,
                                 'sgsn-CAMEL-SubscriptionInfo' = asn1_NOVALUE,
                                 chargingCharacteristics = asn1_NOVALUE,
                                 accessRestrictionData = asn1_NOVALUE,
                                 'ics-Indicator' = asn1_NOVALUE,
                                 'eps-SubscriptionData' = asn1_NOVALUE,
                                 'csg-SubscriptionDataList' = asn1_NOVALUE,
                                 'ue-ReachabilityRequestIndicator' = asn1_NOVALUE,
                                 'sgsn-Number' = asn1_NOVALUE,
                                 'mme-Name' = asn1_NOVALUE}}}}]}}).

-define(MAP_DSD1_DEC_IN,
{'begin',
    #'MapSpecificPDUs_begin'{
        otid = [30,226,70,242],
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',
                        <<96,15,128,2,7,128,161,9,6,7,4,0,0,1,0,16,3>>}},
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,1},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,8},
                         argument = 
                             #'DeleteSubscriberDataArg'{
                                 imsi = [19,64,1,35,35,35,35,248],
                                 basicServiceList = asn1_NOVALUE,'ss-List' = asn1_NOVALUE,
                                 roamingRestrictionDueToUnsupportedFeature = asn1_NOVALUE,
                                 regionalSubscriptionIdentifier = asn1_NOVALUE,
                                 vbsGroupIndication = asn1_NOVALUE,
                                 vgcsGroupIndication = asn1_NOVALUE,
                                 camelSubscriptionInfoWithdraw = 'NULL',
                                 extensionContainer = asn1_NOVALUE,
                                 gprsSubscriptionDataWithdraw = asn1_NOVALUE,
                                 roamingRestrictedInSgsnDueToUnsuppportedFeature = 
                                     asn1_NOVALUE,
                                 lsaInformationWithdraw = asn1_NOVALUE,
                                 'gmlc-ListWithdraw' = asn1_NOVALUE,
                                 istInformationWithdraw = asn1_NOVALUE,
                                 'specificCSI-Withdraw' = asn1_NOVALUE,
                                 chargingCharacteristicsWithdraw = asn1_NOVALUE,
                                 'stn-srWithdraw' = asn1_NOVALUE,
                                 epsSubscriptionDataWithdraw = asn1_NOVALUE,
                                 'apn-oi-replacementWithdraw' = asn1_NOVALUE,
                                 'csg-SubscriptionDeleted' = asn1_NOVALUE}}}}]}}).

-define(MAP_DSD2_DEC_IN,
{'begin',
    #'MapSpecificPDUs_begin'{
        otid = [30,226,70,242],
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',
                        <<96,15,128,2,7,128,161,9,6,7,4,0,0,1,0,16,3>>}},
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,1},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,8},
                         argument = 
                             #'DeleteSubscriberDataArg'{
                                 imsi = [19,64,1,35,35,35,35,248],
                                 basicServiceList = asn1_NOVALUE,'ss-List' = asn1_NOVALUE,
                                 roamingRestrictionDueToUnsupportedFeature = asn1_NOVALUE,
                                 regionalSubscriptionIdentifier = asn1_NOVALUE,
                                 vbsGroupIndication = asn1_NOVALUE,
                                 vgcsGroupIndication = asn1_NOVALUE,
                                 camelSubscriptionInfoWithdraw = asn1_NOVALUE,
                                 extensionContainer = asn1_NOVALUE,
                                 gprsSubscriptionDataWithdraw = asn1_NOVALUE,
                                 roamingRestrictedInSgsnDueToUnsuppportedFeature = 
                                     asn1_NOVALUE,
                                 lsaInformationWithdraw = asn1_NOVALUE,
                                 'gmlc-ListWithdraw' = asn1_NOVALUE,
                                 istInformationWithdraw = asn1_NOVALUE,
                                 'specificCSI-Withdraw' = ['o-csi', 'mo-sms-csi'],
                                 chargingCharacteristicsWithdraw = asn1_NOVALUE,
                                 'stn-srWithdraw' = asn1_NOVALUE,
                                 epsSubscriptionDataWithdraw = asn1_NOVALUE,
                                 'apn-oi-replacementWithdraw' = asn1_NOVALUE,
                                 'csg-SubscriptionDeleted' = asn1_NOVALUE}}}}]}}).

-define(MAP_DSD_DEC_OUT,
{'begin',
    #'MapSpecificPDUs_begin'{
        otid = [30,226,70,242],
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',
                        <<96,15,128,2,7,128,161,9,6,7,4,0,0,1,0,16,3>>}},
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,1},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,8},
                         argument = 
                             #'DeleteSubscriberDataArg'{
                                 imsi = [19,64,1,35,35,35,35,248],
                                 basicServiceList = asn1_NOVALUE,'ss-List' = asn1_NOVALUE,
                                 roamingRestrictionDueToUnsupportedFeature = asn1_NOVALUE,
                                 regionalSubscriptionIdentifier = asn1_NOVALUE,
                                 vbsGroupIndication = asn1_NOVALUE,
                                 vgcsGroupIndication = asn1_NOVALUE,
                                 camelSubscriptionInfoWithdraw = asn1_NOVALUE,
                                 extensionContainer = asn1_NOVALUE,
                                 gprsSubscriptionDataWithdraw = asn1_NOVALUE,
                                 roamingRestrictedInSgsnDueToUnsuppportedFeature = 
                                     asn1_NOVALUE,
                                 lsaInformationWithdraw = asn1_NOVALUE,
                                 'gmlc-ListWithdraw' = asn1_NOVALUE,
                                 istInformationWithdraw = asn1_NOVALUE,
                                 'specificCSI-Withdraw' = asn1_NOVALUE,
                                 chargingCharacteristicsWithdraw = asn1_NOVALUE,
                                 'stn-srWithdraw' = asn1_NOVALUE,
                                 epsSubscriptionDataWithdraw = asn1_NOVALUE,
                                 'apn-oi-replacementWithdraw' = asn1_NOVALUE,
                                 'csg-SubscriptionDeleted' = asn1_NOVALUE}}}}]}}).


setup() ->
	application:set_env(mgw_nat, camel_phase_patch_table, [
			% each element in this list is a tuple of two lists:
			%  first half of the tuple: property-list of #gtt_match field members
			%  second half: list of atoms for camel phase [ phase1, phase2, phase3 ]
			{ [ {gt_range_from,	443850000000000 },
			    {gt_range_to,	443859999999999 } ], [ phase1 ] }
	]),
	application:set_env(mgw_nat, camel_phase_patch_table_outbound, [
			{ [ {gt_range_from,	443850000000000 },
			    {gt_range_to,	443859999999999 } ], [ phase1, phase2, phase3 ] }
	]),
	application:set_env(mgw_nat, mangle_tt_sri_sm_pfx, [ 91 ]),
	application:set_env(mgw_nat, csi_tbl_filename, "/tmp/csi.dets"),
	ets:new(imsi_tbl, [named_table, public]),
	mgw_nat_act_vfuk_onw:reload_config().

teardown(_) ->
	ets:delete(imsi_tbl),
	application:unset_env(mgw_nat, mangle_tt_sri_sm_pfx),
	application:unset_env(mgw_nat, camel_phase_patch_table),
	application:unset_env(mgw_nat, camel_phase_patch_table_outbound),
	application:unset_env(mgw_nat, csi_tbl_filename).

% Test the tuple walker and camelph_twalk_cb() directly, as we don't have a
% SCCP header in front of the MAP message and thus we cannot call
% mangle_map_camel_phase() directly
camelphase_twalk() ->
	?assertEqual(?MAP_LU_DEC_OUT, osmo_util:tuple_walk(?MAP_LU_DEC_IN,
							fun mgw_nat_act_vfuk_onw:camelph_twalk_cb/3,
							[[phase1]])).

build_fake_sccp_msg(CalledDigList, CallingDigList) ->
	Gt_called = #global_title{phone_number = CalledDigList},
	SccpAddr_called = #sccp_addr{global_title = Gt_called},
	Gt_calling = #global_title{phone_number = CallingDigList},
	SccpAddr_calling = #sccp_addr{global_title = Gt_calling},
	#sccp_msg{parameters = [{called_party_addr, SccpAddr_called},
				{calling_party_addr, SccpAddr_calling}]}.

% a full test testing the entire chain...
camelphase_full() ->
	% Set up a fake SCCP message with Called Addr and GT
	SccpDec = build_fake_sccp_msg([4,4,3,8,5,0,0,0,0,0,0,0,0,0,1], [4,9,1,7,1,0,1,2,3,4,5,6]),
	% call the rewrite actor
	MapOut = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_msc, [SccpDec], 0, ?MAP_LU_DEC_IN),
	?assertEqual(?MAP_LU_DEC_OUT, MapOut).

camelphase_full_nomatch() ->
	% Set up a fake SCCP message with Called Addr and GT
	SccpDec = build_fake_sccp_msg([4,4,3,8,6,5,4,3,2,1,2,3,4,5,1], [4,9,1,7,1,0,1,2,3,4,5,6]),
	% call the rewrite actor
	MapOut = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_msc, [SccpDec], 0, ?MAP_LU_DEC_IN),
	?assertEqual(?MAP_LU_DEC_IN, MapOut).

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
		  { timeout, 5*60, ?_test(test_pcap("../../map.pcap")) } ]
	}.

csi_full_lu() ->
	SccpDecFromStp = build_fake_sccp_msg([4,9,1,7,1,0,1,2,3,4,5,6], [4,4,3,8,5,0,0,0,0,0,0,0,0,0,1]),
	MapOutLU = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_stp, [SccpDecFromStp], 0, ?MAP_LU2_DEC_IN),
	?assertEqual(?MAP_LU2_DEC_OUT, MapOutLU).

csi_full_lu_nomatch() ->
	SccpDecFromStp = build_fake_sccp_msg([4,9,1,7,1,0,1,2,3,4,5,6], [6,0,2,3,5,0,0,0,0,0,0,0,9,9,9]),
	MapOutLU = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_stp, [SccpDecFromStp], 0, ?MAP_LU2_DEC_IN),
	?assertEqual(?MAP_LU2_DEC_IN, MapOutLU).

csi_full_isd1() ->
	SccpDecFromMsc = build_fake_sccp_msg([4,4,3,8,5,0,0,0,0,0,0,0,0,0,1], [4,9,1,7,1,0,1,2,3,4,5,6]),
	MapOutISD1 = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_msc, [SccpDecFromMsc], 0, ?MAP_ISD1_DEC_IN),
	?assertEqual(?MAP_ISD1_DEC_IN, MapOutISD1).

csi_full_isd1a() ->
	SccpDecFromMsc = build_fake_sccp_msg([4,4,3,8,5,0,0,0,0,0,0,0,0,0,1], [4,9,1,7,1,0,1,2,3,4,5,6]),
	MapOutISD1A = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_msc, [SccpDecFromMsc], 0, ?MAP_ISD1A_DEC_IN),
	?assertEqual(?MAP_ISD1A_DEC_OUT, MapOutISD1A).

csi_full_isd2() ->
	SccpDecFromMsc = build_fake_sccp_msg([4,4,3,8,5,0,0,0,0,0,0,0,0,0,1], [4,9,1,7,1,0,1,2,3,4,5,6]),
	MapOutISD2 = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_msc, [SccpDecFromMsc], 0, ?MAP_ISD2_DEC_IN),
	?assertEqual(?MAP_ISD2_DEC_OUT, MapOutISD2).

csi_full_isd3() ->
	SccpDecFromMsc = build_fake_sccp_msg([4,4,3,8,5,0,0,0,0,0,0,0,0,0,1], [4,9,1,7,1,0,1,2,3,4,5,6]),
	MapOutISD3 = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_msc, [SccpDecFromMsc], 0, ?MAP_ISD3_DEC_IN),
	?assertEqual(?MAP_ISD3_DEC_OUT, MapOutISD3).

csi_full_dsd1() ->
	SccpDecFromMsc = build_fake_sccp_msg([4,4,3,8,5,0,0,0,0,0,0,0,0,0,1], [4,9,1,7,1,0,1,2,3,4,5,6]),
	MapOutDSD = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_msc, [SccpDecFromMsc], 0, ?MAP_DSD1_DEC_IN),
	?assertEqual(?MAP_DSD_DEC_OUT, MapOutDSD).
	
csi_full_dsd2() ->
	SccpDecFromMsc = build_fake_sccp_msg([4,4,3,8,5,0,0,0,0,0,0,0,0,0,1], [4,9,1,7,1,0,1,2,3,4,5,6]),
	MapOutDSD = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_msc, [SccpDecFromMsc], 0, ?MAP_DSD2_DEC_IN),
	?assertEqual(?MAP_DSD_DEC_OUT, MapOutDSD).
	
csi_test_() ->
	{setup,
		fun setup/0,
		fun teardown/1,
		[ ?_test(csi_full_lu()),
		  ?_test(csi_full_lu_nomatch()),
		  ?_test(csi_full_isd1()),
		  ?_test(csi_full_isd1a()),
		  ?_test(csi_full_isd2()),
		  ?_test(csi_full_dsd1()),
		  ?_test(csi_full_lu()),
		  ?_test(csi_full_isd3()),
		  ?_test(csi_full_dsd2())]
	}.
