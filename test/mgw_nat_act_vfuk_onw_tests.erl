-module(mgw_nat_act_vfuk_onw_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").

-include_lib("osmo_map/include/map.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/osmo_util.hrl").
-include_lib("TCAP/include/TCAPMessages.hrl").


%%%%%%%%%%%%%% export functions so they can be called directly - remove later
-compile(export_all).


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


-define(SCCP_NEW_LU_DEC,
#sccp_msg{
        msg_type = 9,
        parameters = 
            [{protocol_class,{0,8}},
             {called_party_addr,
                 #sccp_addr{
                     res_nat_use = 0,route_on_ssn = 0,point_code = undefined,
                     ssn = 6,
                     global_title = 
                         #global_title{
                             gti = 4,nature_of_addr_ind = 4,trans_type = 0,
                             encoding = undefined,numbering_plan = 1,
                             phone_number = [3,5,4,3,8,5,0,0,0,0,1,1,7,3,1]}}},
             {calling_party_addr,
                 #sccp_addr{
                     res_nat_use = 0,route_on_ssn = 0,point_code = undefined,
                     ssn = 7,
                     global_title = 
                         #global_title{
                             gti = 4,nature_of_addr_ind = 4,trans_type = 0,
                             encoding = undefined,numbering_plan = 1,
                             phone_number = [3,5,4,3,8,5,9,8,9,9,9,8]}}},
             {user_data,
                 <<98,81,72,4,0,0,0,1,107,26,40,24,6,7,0,17,134,5,1,1,
                   1,160,13,96,11,161,9,6,7,4,0,0,1,0,1,3,108,45,161,
                   43,2,1,1,2,1,2,48,35,4,8,114,4,8,0,0,17,55,241,129,
                   7,145,83,52,88,137,153,121,4,7,145,83,132,149,152,
                   153,246,166,5,128,3,0,224,0>>}]}).

-define(MAP_NEW_LU_DEC,
{'begin',
    #'MapSpecificPDUs_begin'{
        otid = [0,0,0,1],
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',<<96,11,161,9,6,7,4,0,0,1,0,1,3>>}},
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,1},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,2},
                         argument = 
                             #'UpdateLocationArg'{
                                 imsi = [114,4,8,0,0,17,55,241],
                                 'msc-Number' = [145,83,52,88,137,153,121],
                                 'vlr-Number' = [145,83,132,149,152,153,246],
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

-define(SCCP_NEW_ISD_DEC,
#sccp_msg{
        msg_type = 9,
        parameters = 
            [{protocol_class,{1,0}},
             {called_party_addr,
                 #sccp_addr{
                     res_nat_use = 0,route_on_ssn = 0,point_code = undefined,
                     ssn = 7,
                     global_title = 
                         #global_title{
                             gti = 4,nature_of_addr_ind = 4,trans_type = 0,
                             encoding = undefined,numbering_plan = 1,
                             phone_number = [3,5,4,3,8,5,9,8,9,9,9,8]}}},
             {calling_party_addr,
                 #sccp_addr{
                     res_nat_use = 0,route_on_ssn = 0,point_code = undefined,
                     ssn = 6,
                     global_title = 
                         #global_title{
                             gti = 4,nature_of_addr_ind = 4,trans_type = 0,
                             encoding = undefined,numbering_plan = 1,
                             phone_number = [3,5,4,3,8,5,0,0,0,0,1,1,7,3,1]}}},
             {user_data,
                 <<101,129,211,72,4,81,1,4,115,73,4,0,0,0,1,107,42,40,
                   40,6,7,0,17,134,5,1,1,1,160,29,97,27,128,2,7,128,
                   161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,163,5,161,3,2,1,
                   0,108,129,152,161,129,149,2,1,65,2,1,7,48,129,140,
                   129,7,145,83,52,88,0,16,145,130,1,10,131,1,1,164,66,
                   4,1,17,4,1,18,4,1,19,4,1,48,4,1,64,4,1,33,4,1,34,4,
                   1,35,4,1,36,4,1,37,4,1,38,4,1,39,4,1,28,4,1,29,4,1,
                   30,4,1,31,4,1,56,4,1,72,4,1,44,4,1,45,4,1,46,4,1,47,
                   166,12,4,1,17,4,1,18,4,1,33,4,1,34,167,34,161,21,4,
                   1,155,48,16,48,6,131,1,32,132,1,0,48,6,131,1,16,132,
                   1,0,163,9,4,1,17,132,1,4,129,1,1,168,5,3,3,1,0,0>>}]}).

-define(MAP_NEW_ISD1_DEC,
{continue,
    #'MapSpecificPDUs_continue'{
        otid = [81,1,4,115],
        dtid = [0,0,0,1],
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
                         invokeId = {present,65},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,7},
                         argument = 
                             #'InsertSubscriberDataArg'{
                                 imsi = asn1_NOVALUE,
                                 msisdn = [145,83,52,88,0,16,145],
                                 category = "\n",
                                 subscriberStatus = operatorDeterminedBarring,
                                 bearerServiceList = 
                                     [[17],
                                      [18],
                                      [19],
                                      "0","@","!","\"","#","$","%","&","'",
                                      [28],
                                      [29],
                                      [30],
                                      [31],
                                      "8","H",",","-",".","/"],
                                 teleserviceList = [[17],[18],"!","\""],
                                 provisionedSS = 
                                     [{callBarringInfo,
                                          #'Ext-CallBarInfo'{
                                              'ss-Code' = [155],
                                              callBarringFeatureList = 
                                                  [#'Ext-CallBarringFeature'{
                                                       basicService = {'ext-Teleservice'," "},
                                                       'ss-Status' = [0],
                                                       extensionContainer = asn1_NOVALUE},
                                                   #'Ext-CallBarringFeature'{
                                                       basicService = {'ext-Teleservice',[16]},
                                                       'ss-Status' = [0],
                                                       extensionContainer = asn1_NOVALUE}],
                                              extensionContainer = asn1_NOVALUE}},
                                      {'ss-Data',
                                          #'Ext-SS-Data'{
                                              'ss-Code' = [17],
                                              'ss-Status' = [4],
                                              'ss-SubscriptionOption' = 
                                                  {overrideCategory,overrideDisabled},
                                              basicServiceGroupList = asn1_NOVALUE,
                                              extensionContainer = asn1_NOVALUE}}],
                                 'odb-Data' = 
                                     #'ODB-Data'{
                                         'odb-GeneralData' = [],'odb-HPLMN-Data' = asn1_NOVALUE,
                                         extensionContainer = asn1_NOVALUE},
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

-define(MAP_NEW_ISD2_DEC,
{continue,
    #'MapSpecificPDUs_continue'{
        otid = [81,1,4,115],
        dtid = [0,0,0,1],
        dialoguePortion = asn1_NOVALUE,
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,66},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,7},
                         argument = 
                             #'InsertSubscriberDataArg'{
                                 imsi = asn1_NOVALUE,msisdn = asn1_NOVALUE,
                                 category = asn1_NOVALUE,subscriberStatus = asn1_NOVALUE,
                                 bearerServiceList = asn1_NOVALUE,
                                 teleserviceList = asn1_NOVALUE,
                                 provisionedSS = 
                                     [{'ss-Data',
                                          #'Ext-SS-Data'{
                                              'ss-Code' = [18],
                                              'ss-Status' = [0],
                                              'ss-SubscriptionOption' = {cliRestrictionOption,permanent},
                                              basicServiceGroupList = asn1_NOVALUE,
                                              extensionContainer = asn1_NOVALUE}},
                                      {'ss-Data',
                                          #'Ext-SS-Data'{
                                              'ss-Code' = [19],
                                              'ss-Status' = [4],
                                              'ss-SubscriptionOption' = 
                                                  {overrideCategory,overrideDisabled},
                                              basicServiceGroupList = asn1_NOVALUE,
                                              extensionContainer = asn1_NOVALUE}},
                                      {'ss-Data',
                                          #'Ext-SS-Data'{
                                              'ss-Code' = [20],
                                              'ss-Status' = [0],
                                              'ss-SubscriptionOption' = asn1_NOVALUE,
                                              basicServiceGroupList = asn1_NOVALUE,
                                              extensionContainer = asn1_NOVALUE}},
                                      {'ss-Data',
                                          #'Ext-SS-Data'{
                                              'ss-Code' = "A",
                                              'ss-Status' = [1],
                                              'ss-SubscriptionOption' = asn1_NOVALUE,
                                              basicServiceGroupList = asn1_NOVALUE,
                                              extensionContainer = asn1_NOVALUE}},
                                      {'ss-Data',
                                          #'Ext-SS-Data'{
                                              'ss-Code' = "B",
                                              'ss-Status' = [4],
                                              'ss-SubscriptionOption' = asn1_NOVALUE,
                                              basicServiceGroupList = asn1_NOVALUE,
                                              extensionContainer = asn1_NOVALUE}},
                                      {'ss-Data',
                                          #'Ext-SS-Data'{
                                              'ss-Code' = "1",
                                              'ss-Status' = [4],
                                              'ss-SubscriptionOption' = asn1_NOVALUE,
                                              basicServiceGroupList = asn1_NOVALUE,
                                              extensionContainer = asn1_NOVALUE}},
                                      {'ss-Data',
                                          #'Ext-SS-Data'{
                                              'ss-Code' = "Q",
                                              'ss-Status' = [4],
                                              'ss-SubscriptionOption' = asn1_NOVALUE,
                                              basicServiceGroupList = asn1_NOVALUE,
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

-define(MAP_NEW_ISD3_DEC,
{continue,
    #'MapSpecificPDUs_continue'{
        otid = [81,1,4,115],
        dtid = [0,0,0,1],
        dialoguePortion = asn1_NOVALUE,
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,67},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,7},
                         argument = 
                             #'InsertSubscriberDataArg'{
                                 imsi = asn1_NOVALUE,msisdn = asn1_NOVALUE,
                                 category = asn1_NOVALUE,subscriberStatus = asn1_NOVALUE,
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
                                                          serviceKey = 0,
                                                          'gsmSCF-Address' = [145,83,52,88,137,153,25],
                                                          defaultCallHandling = releaseCall,
                                                          extensionContainer = asn1_NOVALUE}],
                                                 extensionContainer = asn1_NOVALUE,
                                                 camelCapabilityHandling = 2,
                                                 notificationToCSE = asn1_NOVALUE,csiActive = asn1_NOVALUE},
                                         extensionContainer = asn1_NOVALUE,
                                         'ss-CSI' = 
                                             #'SS-CSI'{
                                                 'ss-CamelData' = 
                                                     #'SS-CamelData'{
                                                         'ss-EventList' = [],
                                                         'gsmSCF-Address' = [145,83,52,88,137,153,25],
                                                         extensionContainer = asn1_NOVALUE},
                                                 extensionContainer = asn1_NOVALUE,
                                                 notificationToCSE = asn1_NOVALUE,
                                                 'csi-Active' = asn1_NOVALUE},
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
                                                          serviceKey = 0,
                                                          'gsmSCF-Address' = [145,83,52,88,137,153,25],
                                                          'defaultSMS-Handling' = continueTransaction,
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

-define(SCCP_NEW_MOFWSM_DEC,
#sccp_msg{
        msg_type = 9,
        parameters = 
            [{protocol_class,{0,8}},
             {called_party_addr,
                 #sccp_addr{
                     res_nat_use = 0,route_on_ssn = 0,point_code = undefined,
                     ssn = 8,
                     global_title = 
                         #global_title{
                             gti = 4,nature_of_addr_ind = 4,trans_type = 0,
                             encoding = undefined,numbering_plan = 1,
                             phone_number = [3,5,4,3,8,5,9,8,9,9,8,5]}}},
             {calling_party_addr,
                 #sccp_addr{
                     res_nat_use = 0,route_on_ssn = 0,point_code = undefined,
                     ssn = 8,
                     global_title = 
                         #global_title{
                             gti = 4,nature_of_addr_ind = 4,trans_type = 0,
                             encoding = undefined,numbering_plan = 1,
                             phone_number = [3,5,4,3,8,5,9,8,9,9,9,7]}}},
             {user_data,
                 <<98,84,72,4,0,0,0,2,107,26,40,24,6,7,0,17,134,5,1,1,
                   1,160,13,96,11,161,9,6,7,4,0,0,1,0,21,3,108,48,161,
                   46,2,1,1,2,1,46,48,38,132,7,145,83,52,88,137,153,88,
                   130,7,145,83,52,88,0,16,145,4,18,17,45,11,145,100,7,
                   146,89,4,244,0,0,255,4,212,242,156,14>>}]}).

-define(MAP_NEW_MOFWSM_DEC,
{'begin',
    #'MapSpecificPDUs_begin'{
        otid = [0,0,0,2],
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',<<96,11,161,9,6,7,4,0,0,1,0,21,3>>}},
        components = 
            [{basicROS,
                 {invoke,
                     #'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{
                         invokeId = {present,1},
                         linkedId = asn1_NOVALUE,
                         opcode = {local,46},
                         argument = 
                             #'MO-ForwardSM-Arg'{
                                 'sm-RP-DA' = 
                                     {serviceCentreAddressDA,[145,83,52,88,137,153,88]},
                                 'sm-RP-OA' = {msisdn,[145,83,52,88,0,16,145]},
                                 'sm-RP-UI' = 
                                     [17,45,11,145,100,7,146,89,4,244,0,0,255,4,212,242,156,14],
                                 extensionContainer = asn1_NOVALUE,imsi = asn1_NOVALUE}}}}]}}).

-define(SCCP_NEW_RETRESL_DEC,
#sccp_msg{
        msg_type = 9,
        parameters = 
            [{protocol_class,{0,0}},
             {called_party_addr,
                 #sccp_addr{
                     res_nat_use = 0,route_on_ssn = 0,point_code = undefined,
                     ssn = 8,
                     global_title = 
                         #global_title{
                             gti = 4,nature_of_addr_ind = 4,trans_type = 0,
                             encoding = undefined,numbering_plan = 1,
                             phone_number = [3,5,4,3,8,5,9,8,9,9,9,7]}}},
             {calling_party_addr,
                 #sccp_addr{
                     res_nat_use = 0,route_on_ssn = 0,point_code = undefined,
                     ssn = 8,
                     global_title = 
                         #global_title{
                             gti = 4,nature_of_addr_ind = 4,trans_type = 0,
                             encoding = undefined,numbering_plan = 1,
                             phone_number = [3,5,4,3,8,5,9,8,9,9,8,5]}}},
             {user_data,
                 <<100,57,73,4,0,0,0,2,107,42,40,40,6,7,0,17,134,5,1,1,
                   1,160,29,97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,21,3,
                   162,3,2,1,0,163,5,161,3,2,1,0,108,5,162,3,2,1,1>>}]}).

-define(MAP_NEW_RETRESL_DEC,
{'end',
    #'MapSpecificPDUs_end'{
        dtid = [0,0,0,2],
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',
                        <<97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,21,3,162,3,2,1,
                          0,163,5,161,3,2,1,0>>}},
        components = 
            [{basicROS,
                 {returnResult,
                     #'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult'{
                         invokeId = {present,1},
                         result = asn1_NOVALUE}}}]}}).

-define(MAP_NEW_ERROR_DEC,
{'end',
    #'MapSpecificPDUs_end'{
        dtid = [0,0,0,2],
        dialoguePortion = 
            #'EXTERNAL'{
                'direct-reference' = {0,0,17,773,1,1,1},
                'indirect-reference' = asn1_NOVALUE,
                'data-value-descriptor' = asn1_NOVALUE,
                encoding = 
                    {'single-ASN1-type',
                        <<97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,21,3,162,3,2,1,
                          0,163,5,161,3,2,1,0>>}},
        components = 
            [{basicROS,
                 {returnError,
                     #'MapSpecificPDUs_end_components_SEQOF_basicROS_returnError'{
                         invokeId = {present,1},
                         errcode = {local,32},
                         parameter = asn1_NOVALUE}}}]}}).


setup_new() ->
	application:set_env(mgw_nat, camel_phase_patch_table, [
			% each element in this list is a tuple of two lists:
			%  first half of the tuple: property-list of #gtt_match field members
			%  second half: list of atoms for camel phase [ phase1, phase2, phase3 ]
			{ [ {gt_range_from,	443850000000000 },
			    {gt_range_to,	443859999999999 } ], [ phase1 ] }
	]),
	application:set_env(mgw_nat, camel_phase_patch_table_outbound, [
			{ [ {gt_range_from,	354385000000 },
			    {gt_range_to,	354385999999 } ], [ phase1, phase2, phase3 ] }
	]),
	application:set_env(mgw_nat, mangle_tt_sri_sm_pfx, [ 91 ]),
	application:set_env(mgw_nat, csi_tbl_filename, "/tmp/csi.dets"),
	application:set_env(mgw_nat, csi_tbl_timeout_mins, 1440),
	application:set_env(mgw_nat, imsi_tbl_timeout_secs, 120),
	application:set_env(mgw_nat, mpid_tbl_timeout_secs, 120),
	application:set_env(fake_smsssf, sms_release_errcode, 42),
	application:set_env(fake_smsssf, fake_msc_gt_bcd, [145,83,52,88,137,153,121]),
	mgw_nat_act_vfuk_onw:init_config(),
	mgw_nat_act_vfuk_onw:reload_config().




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
	application:set_env(mgw_nat, csi_tbl_timeout_mins, 1440),
	mgw_nat_act_vfuk_onw:init_config(),
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

ssf_test1() ->
    Out1 = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_stp, [?SCCP_NEW_LU_DEC], 0, ?MAP_NEW_LU_DEC).
ssf_test2() ->
    Out2 = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_msc, [?SCCP_NEW_ISD_DEC], 0, ?MAP_NEW_ISD1_DEC).
ssf_test3() ->
    Out3 = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_msc, [?SCCP_NEW_ISD_DEC], 0, ?MAP_NEW_ISD2_DEC).
ssf_test4() ->
    Out4 = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_msc, [?SCCP_NEW_ISD_DEC], 0, ?MAP_NEW_ISD3_DEC).
ssf_test5() ->
    Out5 = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_stp, [?SCCP_NEW_MOFWSM_DEC], 0, ?MAP_NEW_MOFWSM_DEC).
ssf_test6() ->
    Out6 = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_msc, [?SCCP_NEW_RETRESL_DEC], 0, ?MAP_NEW_RETRESL_DEC).
ssf_test6a() ->
    Out6 = mgw_nat_act_vfuk_onw:rewrite_actor(map, from_msc, [?SCCP_NEW_RETRESL_DEC], 0, ?MAP_NEW_ERROR_DEC).
