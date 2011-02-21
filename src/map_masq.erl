% MAP masquerading application

% (C) 2010-2011 by Harald Welte <laforge@gnumonks.org>
% (C) 2010-2011 by On-Waves
%
% All Rights Reserved
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License along
% with this program; if not, write to the Free Software Foundation, Inc.,
% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 

-module(map_masq).
-author('Harald Welte <laforge@gnumonks.org>').
%-compile(export_all).

-export([mangle_map/2, config_update/0]).

-include_lib("osmo_map/include/map.hrl").
-include_lib("osmo_ss7/include/isup.hrl").

% Use the MAP address translation table to alter an ISDN-Address-String
patch_map_isdn_addr(asn1_NOVALUE, _Type) ->
	asn1_NOVALUE;
patch_map_isdn_addr(AddrIn, Type) when is_binary(AddrIn) ->
	patch_map_isdn_addr(binary_to_list(AddrIn), Type);
patch_map_isdn_addr(AddrIn, Type) when is_list(AddrIn) ->
	% obtain some configuration data
	{ok, Tbl} = application:get_env(map_rewrite_table),
	{ok, IntPfx} = application:get_env(intern_pfx),
	% Decode the list of octets into an party_number
	AddrInDec = map_codec:parse_addr_string(AddrIn),
	% First we always internationalize the address
	AddrInIntl = mgw_nat:isup_party_internationalize(AddrInDec, IntPfx),
	% And then patch/replace the address digits
	DigitsIn = AddrInIntl#party_number.phone_number,
	DigitsOut = patch_map_isdn_digits(DigitsIn, Type, Tbl),
	AddrOutIntl = AddrInIntl#party_number{phone_number = DigitsOut},
	if AddrOutIntl == AddrInDec ->
		ok;
	   true ->
		io:format("Translating MAP-ISDN-Addess ~p ~p -> ~p~n",
			  [Type, AddrInDec, AddrOutIntl])
	end,
	map_codec:encode_addr_string(AddrOutIntl).

patch_map_isdn_digits(AddrIn, _Type, []) ->
	AddrIn;
patch_map_isdn_digits(AddrIn, TypeIn, [Head|Tail]) ->
	case Head of
		{TypeIn, _,_, MscSide, StpSide} ->
			if AddrIn == MscSide ->
				StpSide;
			   AddrIn == StpSide ->
				MscSide;
			true ->
				patch_map_isdn_digits(AddrIn, TypeIn, Tail)
			end;
		_ ->
			patch_map_isdn_digits(AddrIn, TypeIn, Tail)
	end.

mangle_msisdn(from_stp, _Opcode, AddrIn) ->
	{ok, IntPfx} = application:get_env(intern_pfx),
	mgw_nat:isup_party_internationalize(AddrIn, IntPfx).

% Someobdy inquires on Routing Info for a MS (from HLR)
patch(#'SendRoutingInfoArg'{msisdn = Msisdn,'gmsc-OrGsmSCF-Address'=GmscAddr} = P) ->
	% First Translate the MSISDN into international
	AddrInDec = map_codec:parse_addr_string(Msisdn),
	io:format("MSISDN IN = ~p~n", [AddrInDec]),
	AddrOutDec = mangle_msisdn(from_stp, 22, AddrInDec),
	io:format("MSISDN OUT = ~p~n", [AddrOutDec]),
	AddrOutBin = map_codec:encode_addr_string(AddrOutDec),
	% Second, try to masquerade the G-MSC
	GmscInDec = map_codec:parse_addr_string(GmscAddr),
	case sccp_masq:lookup_masq_addr(orig, GmscInDec#party_number.phone_number) of
		undef ->
			GmscOut = GmscAddr;
		GmscOutDigits ->
			GmscOutDec = GmscInDec#party_number{phone_number = GmscOutDigits},
			GmscOut = map_codec:encode_addr_string(GmscOutDec)
	end,
	P#'SendRoutingInfoArg'{msisdn = AddrOutBin, 'gmsc-OrGsmSCF-Address' = GmscOut};

% HLR responds with Routing Info for a MS
patch(#'SendRoutingInfoRes'{extendedRoutingInfo = ExtRoutInfo,
			    'vmsc-Address' = VmscAddress} = P) ->
	VmscAddrOut = patch_map_isdn_addr(VmscAddress, msc),
	P#'SendRoutingInfoRes'{extendedRoutingInfo = patch(ExtRoutInfo),
			       'vmsc-Address' = VmscAddrOut};
patch(#'CamelRoutingInfo'{gmscCamelSubscriptionInfo = GmscCamelSI} = P) ->
	P#'CamelRoutingInfo'{gmscCamelSubscriptionInfo = patch(GmscCamelSI)};
patch({camelRoutingInfo, CRI}) ->
	{camelRoutingInfo, patch(CRI)};
patch({routingInfo, RI}) ->
	{routingInfo, patch(RI)};

% HLR responds to inquiring MSC indicating the current serving MSC number
patch(#'RoutingInfoForSM-Res'{locationInfoWithLMSI = LocInf} = P) ->
	P#'RoutingInfoForSM-Res'{locationInfoWithLMSI = patch(LocInf)};
patch(#'LocationInfoWithLMSI'{'networkNode-Number' = NetNodeNr} = P) ->
	NetNodeNrOut = patch_map_isdn_addr(NetNodeNr, msc),
	P#'LocationInfoWithLMSI'{'networkNode-Number' = NetNodeNrOut};

% patch the roaming number as it is sent from HLR to G-MSC (SRI Resp)
patch({roamingNumber, RoamNumTBCD}) ->
	RoamNumIn = map_codec:parse_addr_string(RoamNumTBCD),
	io:format("Roaming Number IN = ~p~n", [RoamNumIn]),
	{ok, MsrnPfxStp} = application:get_env(msrn_pfx_stp),
	{ok, MsrnPfxMsc} = application:get_env(msrn_pfx_msc),
	RoamNumOut = mgw_nat:isup_party_replace_prefix(RoamNumIn, MsrnPfxMsc, MsrnPfxStp),
	io:format("Roaming Number OUT = ~p~n", [RoamNumOut]),
	RoamNumOutTBCD = map_codec:encode_addr_string(RoamNumOut),
	{roamingNumber, RoamNumOutTBCD};


% patch a UpdateGprsLocationArg and replace SGSN number and SGSN address
% !!! TESTING ONLY !!!
patch(#'UpdateGprsLocationArg'{'sgsn-Number' = SgsnNum,
			       'sgsn-Address' = SgsnAddr} = P) ->
	SgsnNumOut = patch_map_isdn_addr(SgsnNum, sgsn),
	P#'UpdateGprsLocationArg'{'sgsn-Number'= SgsnNumOut,
				  'sgsn-Address' = SgsnAddr};

% Some other SGSN is sendingu us a GPRS location update.  In the response,
% we indicate teh HLR number, which we need to masquerade
patch(#'UpdateGprsLocationRes'{'hlr-Number' = HlrNum} = P) ->
	HlrNumOut = patch_map_isdn_addr(HlrNum, hlr),
	P#'UpdateGprsLocationRes'{'hlr-Number' = HlrNumOut};

% Some other MSC/VLR is sendingu us a GSM location update.  In the response,
% we indicate teh HLR number, which we need to masquerade
patch(#'UpdateLocationRes'{'hlr-Number' = HlrNum} = P) ->
	HlrNumOut = patch_map_isdn_addr(HlrNum, hlr),
	P#'UpdateLocationRes'{'hlr-Number' = HlrNumOut};

% HLR responds to VLR's MAP_RESTORE_REQ (i.e. it has lost information)
patch(#'RestoreDataRes'{'hlr-Number' = HlrNum} = P) ->
	HlrNumOut = patch_map_isdn_addr(HlrNum, hlr),
	P#'RestoreDataRes'{'hlr-Number' = HlrNumOut};

% HLR sends subscriber data to VLR/SGSN, including CAMEL info
patch(#'InsertSubscriberDataArg'{'vlrCamelSubscriptionInfo'=VlrCamel,
				 'sgsn-CAMEL-SubscriptionInfo'=SgsnCamel} = Arg) ->
	Arg#'InsertSubscriberDataArg'{'vlrCamelSubscriptionInfo'=patch(VlrCamel),
				      'sgsn-CAMEL-SubscriptionInfo'=patch(SgsnCamel)};

% HLR sends subscriber data to gsmSCF
patch(#'AnyTimeSubscriptionInterrogationRes'{'camel-SubscriptionInfo'=Csi} = P) ->
	P#'AnyTimeSubscriptionInterrogationRes'{'camel-SubscriptionInfo'=patch(Csi)};

patch(asn1_NOVALUE) ->
	asn1_NOVALUE;

% CAMEL related parsing

% this is part of the SRI Response (HLR->GMSC)
patch(#'GmscCamelSubscriptionInfo'{'o-CSI'=Ocsi, 't-CSI'=Tcsi,
				   'd-csi'=Dcsi} = P) ->
	P#'GmscCamelSubscriptionInfo'{'o-CSI'=patch(Ocsi),
				      't-CSI'=patch(Tcsi),
				      'd-csi'=patch(Dcsi)};

% this is part of the InsertSubscriberData HLR -> VLR
patch(#'VlrCamelSubscriptionInfo'{'o-CSI'=Ocsi, 'mo-sms-CSI'=MoSmsCsi,
				  'mt-sms-CSI'=MtSmsCsi, 'ss-CSI'=SsCsi} = P) ->
	P#'VlrCamelSubscriptionInfo'{'o-CSI'=patch(Ocsi),
				    'mo-sms-CSI'=patch(MoSmsCsi),
				    'mt-sms-CSI'=patch(MtSmsCsi),
				    'ss-CSI'=patch(SsCsi)};

% this is part of the InsertSubscriberData HLR -> SGSN
patch(#'SGSN-CAMEL-SubscriptionInfo'{'gprs-CSI'=GprsCsi,
				     'mo-sms-CSI'=MoSmsCsi,
				     'mt-sms-CSI'=MtSmsCsi} = P) ->
	P#'SGSN-CAMEL-SubscriptionInfo'{'gprs-CSI'=patch(GprsCsi),
					'mo-sms-CSI'=patch(MoSmsCsi),
					'mt-sms-CSI'=patch(MtSmsCsi)};

% this is part of the Anytime Subscription Interrogation Result HLR->gsmSCF
patch(#'CAMEL-SubscriptionInfo'{'o-CSI'=Ocsi,
				'd-CSI'=Dcsi,
				't-CSI'=Tcsi,
				'vt-CSI'=Vtcsi,
				%'tif-CSI'=Tifcsi,
				'gprs-CSI'=GprsCsi,
				'mo-sms-CSI'=MoSmsCsi,
				'ss-CSI'=SsCsi,
				'm-CSI'=Mcsi,
				'mt-sms-CSI'=MtSmsCsi,
				'mg-csi'=MgCsi,
				'o-IM-CSI'=OimCsi,
				'd-IM-CSI'=DimCsi,
				'vt-IM-CSI'=VtImCsi} = P) ->
	P#'CAMEL-SubscriptionInfo'{'o-CSI'=patch(Ocsi),
				'd-CSI'=patch(Dcsi),
				't-CSI'=patch(Tcsi),
				'vt-CSI'=patch(Vtcsi),
				'gprs-CSI'=patch(GprsCsi),
				'mo-sms-CSI'=patch(MoSmsCsi),
				'ss-CSI'=patch(SsCsi),
				'm-CSI'=patch(Mcsi),
				'mt-sms-CSI'=patch(MtSmsCsi),
				'mg-csi'=patch(MgCsi),
				'o-IM-CSI'=patch(OimCsi),
				'd-IM-CSI'=patch(DimCsi),
				'vt-IM-CSI'=patch(VtImCsi)};

patch(#'T-CSI'{'t-BcsmCamelTDPDataList'=TdpList} = P) ->
	P#'T-CSI'{'t-BcsmCamelTDPDataList'=patch_tBcsmCamelTDPDataList(TdpList)};
patch(#'M-CSI'{'gsmSCF-Address'=GsmScfAddr} = P) ->
	GsmScfAddrOut = patch_map_isdn_addr(GsmScfAddr, scf),
	P#'M-CSI'{'gsmSCF-Address'=GsmScfAddrOut};
patch(#'MG-CSI'{'gsmSCF-Address'=GsmScfAddr} = P) ->
	GsmScfAddrOut = patch_map_isdn_addr(GsmScfAddr, scf),
	P#'MG-CSI'{'gsmSCF-Address'=GsmScfAddrOut};
patch(#'O-CSI'{'o-BcsmCamelTDPDataList'=TdpList} = P) ->
	P#'O-CSI'{'o-BcsmCamelTDPDataList'=patch_oBcsmCamelTDPDataList(TdpList)};
patch(#'D-CSI'{'dp-AnalysedInfoCriteriaList'=List} = P) ->
	P#'D-CSI'{'dp-AnalysedInfoCriteriaList'=patch_AnInfoCritList(List)};
patch(#'SMS-CSI'{'sms-CAMEL-TDP-DataList'=TdpList} = P) ->
	P#'SMS-CSI'{'sms-CAMEL-TDP-DataList'=patch_SmsCamelTDPDataList(TdpList)};
patch(#'SS-CSI'{'ss-CamelData'=Sscd} = P) ->
	P#'SS-CSI'{'ss-CamelData'=patch(Sscd)};
patch(#'GPRS-CSI'{'gprs-CamelTDPDataList'=TdpList} = P) ->
	P#'GPRS-CSI'{'gprs-CamelTDPDataList'=patch_GprsCamelTDPDataList(TdpList)};
patch(#'SS-CamelData'{'gsmSCF-Address'=GsmScfAddr} = P) ->
	GsmScfAddrOut = patch_map_isdn_addr(GsmScfAddr, scf),
	P#'SS-CamelData'{'gsmSCF-Address'=GsmScfAddrOut};
patch(#'O-BcsmCamelTDPData'{'gsmSCF-Address'=GsmScfAddr} = P) ->
	GsmScfAddrOut = patch_map_isdn_addr(GsmScfAddr, scf),
	P#'O-BcsmCamelTDPData'{'gsmSCF-Address'=GsmScfAddrOut};
patch(#'T-BcsmCamelTDPData'{'gsmSCF-Address'=GsmScfAddr} = P) ->
	GsmScfAddrOut = patch_map_isdn_addr(GsmScfAddr, scf),
	P#'T-BcsmCamelTDPData'{'gsmSCF-Address'=GsmScfAddrOut};
patch(#'SMS-CAMEL-TDP-Data'{'gsmSCF-Address'=GsmScfAddr} = P) ->
	GsmScfAddrOut = patch_map_isdn_addr(GsmScfAddr, scf),
	P#'SMS-CAMEL-TDP-Data'{'gsmSCF-Address'=GsmScfAddrOut};
patch(#'GPRS-CamelTDPData'{'gsmSCF-Address'=GsmScfAddr} = P) ->
	GsmScfAddrOut = patch_map_isdn_addr(GsmScfAddr, scf),
	P#'GPRS-CamelTDPData'{'gsmSCF-Address'=GsmScfAddrOut};
patch(#'DP-AnalysedInfoCriterium'{'gsmSCF-Address'=GsmScfAddr} = P) ->
	GsmScfAddrOut = patch_map_isdn_addr(GsmScfAddr, scf),
	P#'DP-AnalysedInfoCriterium'{'gsmSCF-Address'=GsmScfAddrOut};
patch(Default) ->
	Default.

patch_oBcsmCamelTDPDataList(List) ->
	% we reverse the origianl list, as the tail recursive _acc function
	% will invert the order of components again
	patch_oBcsmCamelTDPDataList_acc(lists:reverse(List), []).
patch_oBcsmCamelTDPDataList_acc([], NewList) -> NewList;
patch_oBcsmCamelTDPDataList_acc([TdpData|Tail], NewList) ->
	NewTdpData = patch(TdpData#'O-BcsmCamelTDPData'{}),
	patch_oBcsmCamelTDPDataList_acc(Tail, [NewTdpData|NewList]).

patch_tBcsmCamelTDPDataList(List) ->
	% we reverse the origianl list, as the tail recursive _acc function
	% will invert the order of components again
	patch_tBcsmCamelTDPDataList_acc(lists:reverse(List), []).
patch_tBcsmCamelTDPDataList_acc([], NewList) -> NewList;
patch_tBcsmCamelTDPDataList_acc([TdpData|Tail], NewList) ->
	NewTdpData = patch(TdpData#'T-BcsmCamelTDPData'{}),
	patch_tBcsmCamelTDPDataList_acc(Tail, [NewTdpData|NewList]).

patch_AnInfoCritList(List) ->
	% we reverse the origianl list, as the tail recursive _acc function
	% will invert the order of components again
	patch_AnInfoCritList_acc(lists:reverse(List), []).
patch_AnInfoCritList_acc([], NewList) -> NewList;
patch_AnInfoCritList_acc([Crit|Tail], NewList) ->
	NewCrit = patch(Crit#'DP-AnalysedInfoCriterium'{}),
	patch_AnInfoCritList_acc(Tail, [NewCrit|NewList]).

patch_GprsCamelTDPDataList(List) ->
	% we reverse the origianl list, as the tail recursive _acc function
	% will invert the order of components again
	patch_GprsCamelTDPDataList_acc(lists:reverse(List), []).
patch_GprsCamelTDPDataList_acc([], NewList) -> NewList;
patch_GprsCamelTDPDataList_acc([TdpData|Tail], NewList) ->
	NewTdpData = patch(TdpData#'GPRS-CamelTDPData'{}),
	patch_GprsCamelTDPDataList_acc(Tail, [NewTdpData|NewList]).

patch_SmsCamelTDPDataList(List) ->
	% we reverse the origianl list, as the tail recursive _acc function
	% will invert the order of components again
	patch_SmsCamelTDPDataList_acc(lists:reverse(List), []).
patch_SmsCamelTDPDataList_acc([], NewList) -> NewList;
patch_SmsCamelTDPDataList_acc([TdpData|Tail], NewList) ->
	NewTdpData = patch(TdpData#'SMS-CAMEL-TDP-Data'{}),
	patch_GprsCamelTDPDataList_acc(Tail, [NewTdpData|NewList]).



% process the Argument of a particular MAP invocation
process_component_arg(_From, OpCode, Arg) ->
	case Arg of
		asn1_NOVALUE -> Arg;
		_ -> patch(Arg)
	end.

% recurse over all components
handle_tcap_components(_From, asn1_NOVALUE) ->
	asn1_NOVALUE;
handle_tcap_components(From, List) ->
	% we reverse the origianl list, as the tail recursive _acc function
	% will invert the order of components again
	handle_tcap_components_acc(From, lists:reverse(List), []).
handle_tcap_components_acc(_From, [], NewComponents) -> NewComponents;
handle_tcap_components_acc(From, [Component|Tail], NewComponents) ->
	case Component of
		{basicROS, {Primitive, Body}} ->
			io:format("handle component ~p primitive ~n", [Component]),
			case Body of
				% BEGIN
				#'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{opcode={local, OpCode},
											  argument=Arg} ->
					NewArg = process_component_arg(From, OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{argument=NewArg};
				#'MapSpecificPDUs_begin_components_SEQOF_basicROS_returnResult'{result=#'MapSpecificPDUs_begin_components_SEQOF_basicROS_returnResult_result'{opcode={local, OpCode}, result=Arg} = R} ->
					NewArg = process_component_arg(From, OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_begin_components_SEQOF_basicROS_returnResult'{result=R#'MapSpecificPDUs_begin_components_SEQOF_basicROS_returnResult_result'{result=NewArg}};
				#'MapSpecificPDUs_begin_components_SEQOF_returnResultNotLast'{result=#'MapSpecificPDUs_begin_components_SEQOF_returnResultNotLast_result'{opcode={local, OpCode}, result=Arg} = R} ->
					NewArg = process_component_arg(From, OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_begin_components_SEQOF_returnResultNotLast'{result=R#'MapSpecificPDUs_begin_components_SEQOF_returnResultNotLast_result'{result=NewArg}};
				% END
				#'MapSpecificPDUs_end_components_SEQOF_basicROS_invoke'{opcode={local, OpCode},
											argument=Arg} ->
					NewArg = process_component_arg(From, OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_end_components_SEQOF_basicROS_invoke'{argument=NewArg};
				#'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult'{result=#'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result'{opcode={local, OpCode}, result=Arg} = R} ->
					NewArg = process_component_arg(From, OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult'{result=R#'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result'{result=NewArg}};
				#'MapSpecificPDUs_end_components_SEQOF_returnResultNotLast'{result=#'MapSpecificPDUs_end_components_SEQOF_returnResultNotLast_result'{opcode={local, OpCode}, result=Arg} = R} ->
					NewArg = process_component_arg(From, OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_end_components_SEQOF_returnResultNotLast'{result=R#'MapSpecificPDUs_end_components_SEQOF_returnResultNotLast_result'{result=NewArg}};
				% CONTINUE
				#'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{opcode={local, OpCode},
											     argument=Arg} ->
					NewArg = process_component_arg(From, OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{argument=NewArg};
				#'MapSpecificPDUs_continue_components_SEQOF_basicROS_returnResult'{result=#'MapSpecificPDUs_continue_components_SEQOF_basicROS_returnResult_result'{opcode={local, OpCode}, result=Arg} = R} ->
					NewArg = process_component_arg(From, OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult'{result=R#'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result'{result=NewArg}};
				#'MapSpecificPDUs_continue_components_SEQOF_returnResultNotLast'{result=#'MapSpecificPDUs_continue_components_SEQOF_returnResultNotLast_result'{opcode={local, OpCode}, result=Arg} = R} ->
					NewArg = process_component_arg(From, OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_continue_components_SEQOF_returnResultNotLast'{result=R#'MapSpecificPDUs_continue_components_SEQOF_returnResultNotLast_result'{result=NewArg}};
				_ ->
					NewBody = Body
			end,
			%NewBody = setelement(5, Body, NewArg)
			NewComponent = {basicROS, {Primitive, NewBody}};
		_ ->
			NewComponent = Component
	end,
	io:format("=> modified component ~p~n", [NewComponent]),
	handle_tcap_components_acc(From, Tail, [NewComponent|NewComponents]).
	

% Erlang asn1rt has this strange property that all incoming EXTERNAL types are
% converted from the 1990 version into the 1994 version.  The latter does not
% preserve the encoding (octet string, single ASN1 type, ...).  During encoding,
% it then uses the OCTTET-STRING encoding, which is different from the MAP
% customary single-ASN1-type format.
asn1_EXTERNAL1994_fixup({'EXTERNAL', DirRef, IndRef, Data}) when is_list(Data);is_binary(Data) ->
	% our trick is as follows: we simply convert back to 1990 format, and explicitly
	% set the single-ASN1-type encoding.  asn1rt:s 'enc_EXTERNAL'() will detect this
	#'EXTERNAL'{'direct-reference' = DirRef, 'indirect-reference' = IndRef,
		    'encoding' = {'single-ASN1-type', Data}};
asn1_EXTERNAL1994_fixup(Foo) ->
	Foo.


handle_tcap_dialogue(_From, Foo = {'EXTERNAL', DirRef, IndRef, Data}) ->
	asn1_EXTERNAL1994_fixup(Foo);
handle_tcap_dialogue(_From, Foo) ->
	Foo.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Actual mangling of the decoded MAP messages 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mangle_map(From, {Type, TcapMsgDec}) ->
	case {Type, TcapMsgDec} of
	{'unidirectional', #'MapSpecificPDUs_unidirectional'{dialoguePortion=Dialg,
							     components=Components}} ->
		NewDialg = handle_tcap_dialogue(From, Dialg),
		NewComponents = handle_tcap_components(From, Components),
		NewTcapMsgDec = TcapMsgDec#'MapSpecificPDUs_unidirectional'{dialoguePortion=NewDialg, components=NewComponents};
	{'begin', #'MapSpecificPDUs_begin'{dialoguePortion=Dialg, components=Components}} ->
		NewDialg = handle_tcap_dialogue(From, Dialg),
		NewComponents = handle_tcap_components(From, Components),
		NewTcapMsgDec = TcapMsgDec#'MapSpecificPDUs_begin'{dialoguePortion=NewDialg, components=NewComponents};
	{'continue', #'MapSpecificPDUs_continue'{dialoguePortion=Dialg, components=Components}} ->
		NewDialg = handle_tcap_dialogue(From, Dialg),
		NewComponents = handle_tcap_components(From, Components),
		NewTcapMsgDec = TcapMsgDec#'MapSpecificPDUs_continue'{dialoguePortion=NewDialg, components=NewComponents};
	{'end', #'MapSpecificPDUs_end'{dialoguePortion=Dialg, components=Components}} ->
		NewDialg = handle_tcap_dialogue(From, Dialg),
		NewComponents = handle_tcap_components(From, Components),
		NewTcapMsgDec = TcapMsgDec#'MapSpecificPDUs_end'{dialoguePortion=NewDialg, components=NewComponents};
	%{_, #'Abort'{reason=Reason} ->
	_ ->
		NewTcapMsgDec = TcapMsgDec
	end,
	io:format("new TcapMsgDec (Type=~p) ~p~n", [Type, NewTcapMsgDec]),
	{Type, NewTcapMsgDec}.


% Configuration file has changed, re-generate internal data structures
config_update() ->
	% (re-)generate the MAP Address rewrite table
	{ok, MapRewriteTbl} = application:get_env(mgw_nat, map_rewrite_table),
	MapRewriteTblOut = generate_rewrite_table(MapRewriteTbl),
	application:set_env(mgw_nat, map_rewrite_table, MapRewriteTblOut),
	%{ok, MsrnPfxStp} = application:get_env(msrn_pfx_stp),
	%{ok, MsrnPfxMsc} = application:get_env(msrn_pfx_msc),
	%{ok, IntPfx} = application:get_env(intern_pfx),
	ok.

% Generate the full MAP address rewrite table
generate_rewrite_table(List) when is_list(List) ->
	generate_rewrite_table(List, []).
generate_rewrite_table([], OutList) ->
	io:format("(Re)generated MAP ISDN-Address rewrite table: ~p~n", [OutList]),
	OutList;
generate_rewrite_table([Head|Tail], OutList) ->
	NewItem = generate_rewrite_entry(Head),
	generate_rewrite_table(Tail, [NewItem|OutList]).

% Generate a MAP Address rewrite table entry
generate_rewrite_entry({Name, MscSideInt, StpSideInt}) ->
	MscSideList = osmo_util:int2digit_list(MscSideInt),
	StpSideList = osmo_util:int2digit_list(StpSideInt),
	{Name, MscSideInt, StpSideInt, MscSideList, StpSideList}.
