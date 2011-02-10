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

-export([mangle_map/2]).

-define(PATCH_HLR_NUMBER, [1]).
-define(PATCH_SGSN_NUMBER, [2]).
-define(PATCH_SGSN_ADDRESS, [3]).
-define(PATCH_VMSC_ADDRESS, [4]).
-define(PATCH_GSMSCF_ADDRESS, [5]).

-include_lib("osmo_map/include/map.hrl").

mangle_msisdn(from_stp, _Opcode, AddrIn) ->
	{ok, IntPfx} = application:get_env(intern_pfx),
	mgw_nat:isup_party_internationalize(AddrIn, IntPfx).

% Someobdy inquires on Routing Info for a MS (from HLR)
patch(#'SendRoutingInfoArg'{msisdn = Msisdn} = P) ->
	AddrInDec = map_codec:parse_addr_string(Msisdn),
	io:format("MSISDN IN = ~p~n", [AddrInDec]),
	AddrOutDec = mangle_msisdn(from_stp, 22, AddrInDec),
	io:format("MSISDN OUT = ~p~n", [AddrOutDec]),
	AddrOutBin = map_codec:encode_addr_string(AddrOutDec),
	P#'SendRoutingInfoArg'{msisdn = AddrOutBin};

% HLR responds with Routing Info for a MS
patch(#'SendRoutingInfoRes'{extendedRoutingInfo = ExtRoutInfo,
			    'vmsc-Address' = VmscAddress} = P) ->
	P#'SendRoutingInfoRes'{extendedRoutingInfo = patch(ExtRoutInfo),
			       'vmsc-Address' = ?PATCH_VMSC_ADDRESS};
patch(#'CamelRoutingInfo'{gmscCamelSubscriptionInfo = GmscCamelSI} = P) ->
	P#'CamelRoutingInfo'{gmscCamelSubscriptionInfo = patch(GmscCamelSI)};
patch({camelRoutingInfo, CRI}) ->
	{camelRoutingInfo, patch(CRI)};
patch({routingInfo, RI}) ->
	{routingInfo, patch(RI)};

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
patch(#'UpdateGprsLocationArg'{} = P) ->
	P#'UpdateGprsLocationArg'{'sgsn-Number'= ?PATCH_SGSN_NUMBER,
				  'sgsn-Address' = ?PATCH_SGSN_ADDRESS};

% Some other SGSN is sendingu us a GPRS location update.  In the response,
% we indicate teh HLR number, which we need to masquerade
patch(#'UpdateGprsLocationRes'{} = P) ->
	P#'UpdateGprsLocationRes'{'hlr-Number' = ?PATCH_HLR_NUMBER};

% Some other MSC/VLR is sendingu us a GSM location update.  In the response,
% we indicate teh HLR number, which we need to masquerade
patch(#'UpdateLocationRes'{} = P) ->
	P#'UpdateLocationRes'{'hlr-Number' = ?PATCH_HLR_NUMBER};

% HLR responds to VLR's MAP_RESTORE_REQ (i.e. it has lost information)
patch(#'RestoreDataRes'{} = P) ->
	P#'RestoreDataRes'{'hlr-Number' = ?PATCH_HLR_NUMBER};

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
	P#'M-CSI'{'gsmSCF-Address'=?PATCH_GSMSCF_ADDRESS};
patch(#'MG-CSI'{'gsmSCF-Address'=GsmScfAddr} = P) ->
	P#'MG-CSI'{'gsmSCF-Address'=?PATCH_GSMSCF_ADDRESS};
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
	P#'SS-CamelData'{'gsmSCF-Address'=?PATCH_GSMSCF_ADDRESS};
patch(#'O-BcsmCamelTDPData'{'gsmSCF-Address'=GsmScfAddr} = P) ->
	P#'O-BcsmCamelTDPData'{'gsmSCF-Address'=?PATCH_GSMSCF_ADDRESS};
patch(#'SMS-CAMEL-TDP-Data'{'gsmSCF-Address'=GsmScfAddr} = P) ->
	P#'SMS-CAMEL-TDP-Data'{'gsmSCF-Address'=?PATCH_GSMSCF_ADDRESS};
patch(#'GPRS-CamelTDPData'{'gsmSCF-Address'=GsmScfAddr} = P) ->
	P#'GPRS-CamelTDPData'{'gsmSCF-Address'=?PATCH_GSMSCF_ADDRESS};
patch(#'DP-AnalysedInfoCriterium'{'gsmSCF-Address'=GsmScfAddr} = P) ->
	P#'DP-AnalysedInfoCriterium'{'gsmSCF-Address'=?PATCH_GSMSCF_ADDRESS};
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
process_component_arg(OpCode, Arg) ->
	case Arg of
		asn1_NOVALUE -> Arg;
		_ -> patch(Arg)
	end.

% recurse over all components
handle_tcap_components(asn1_NOVALUE) ->
	asn1_NOVALUE;
handle_tcap_components(List) ->
	% we reverse the origianl list, as the tail recursive _acc function
	% will invert the order of components again
	handle_tcap_components_acc(lists:reverse(List), []).
handle_tcap_components_acc([], NewComponents) -> NewComponents;
handle_tcap_components_acc([Component|Tail], NewComponents) ->
	case Component of
		{basicROS, {Primitive, Body}} ->
			io:format("handle component ~p primitive ~n", [Component]),
			case Body of
				% BEGIN
				#'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{opcode={local, OpCode},
											  argument=Arg} ->
					NewArg = process_component_arg(OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_begin_components_SEQOF_basicROS_invoke'{argument=NewArg};
				#'MapSpecificPDUs_begin_components_SEQOF_basicROS_returnResult'{result=#'MapSpecificPDUs_begin_components_SEQOF_basicROS_returnResult_result'{opcode={local, OpCode}, result=Arg} = R} ->
					NewArg = process_component_arg(OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_begin_components_SEQOF_basicROS_returnResult'{result=R#'MapSpecificPDUs_begin_components_SEQOF_basicROS_returnResult_result'{result=NewArg}};
				#'MapSpecificPDUs_begin_components_SEQOF_returnResultNotLast'{result=#'MapSpecificPDUs_begin_components_SEQOF_returnResultNotLast_result'{opcode={local, OpCode}, result=Arg} = R} ->
					NewArg = process_component_arg(OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_begin_components_SEQOF_returnResultNotLast'{result=R#'MapSpecificPDUs_begin_components_SEQOF_returnResultNotLast_result'{result=NewArg}};
				% END
				#'MapSpecificPDUs_end_components_SEQOF_basicROS_invoke'{opcode={local, OpCode},
											argument=Arg} ->
					NewArg = process_component_arg(OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_end_components_SEQOF_basicROS_invoke'{argument=NewArg};
				#'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult'{result=#'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result'{opcode={local, OpCode}, result=Arg} = R} ->
					NewArg = process_component_arg(OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult'{result=R#'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result'{result=NewArg}};
				#'MapSpecificPDUs_end_components_SEQOF_returnResultNotLast'{result=#'MapSpecificPDUs_end_components_SEQOF_returnResultNotLast_result'{opcode={local, OpCode}, result=Arg} = R} ->
					NewArg = process_component_arg(OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_end_components_SEQOF_returnResultNotLast'{result=R#'MapSpecificPDUs_end_components_SEQOF_returnResultNotLast_result'{result=NewArg}};
				% CONTINUE
				#'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{opcode={local, OpCode},
											     argument=Arg} ->
					NewArg = process_component_arg(OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke'{argument=NewArg};
				#'MapSpecificPDUs_continue_components_SEQOF_basicROS_returnResult'{result=#'MapSpecificPDUs_continue_components_SEQOF_basicROS_returnResult_result'{opcode={local, OpCode}, result=Arg} = R} ->
					NewArg = process_component_arg(OpCode, Arg),
					NewBody = Body#'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult'{result=R#'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result'{result=NewArg}};
				#'MapSpecificPDUs_continue_components_SEQOF_returnResultNotLast'{result=#'MapSpecificPDUs_continue_components_SEQOF_returnResultNotLast_result'{opcode={local, OpCode}, result=Arg} = R} ->
					NewArg = process_component_arg(OpCode, Arg),
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
	handle_tcap_components_acc(Tail, [NewComponent|NewComponents]).
	

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


handle_tcap_dialogue(Foo = {'EXTERNAL', DirRef, IndRef, Data}) ->
	asn1_EXTERNAL1994_fixup(Foo);
handle_tcap_dialogue(Foo) ->
	Foo.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Actual mangling of the decoded MAP messages 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mangle_map(_From, {Type, TcapMsgDec}) ->
	case {Type, TcapMsgDec} of
	{'unidirectional', #'MapSpecificPDUs_unidirectional'{dialoguePortion=Dialg,
							     components=Components}} ->
		NewDialg = handle_tcap_dialogue(Dialg),
		NewComponents = handle_tcap_components(Components),
		NewTcapMsgDec = TcapMsgDec#'MapSpecificPDUs_unidirectional'{dialoguePortion=NewDialg, components=NewComponents};
	{'begin', #'MapSpecificPDUs_begin'{dialoguePortion=Dialg, components=Components}} ->
		NewDialg = handle_tcap_dialogue(Dialg),
		NewComponents = handle_tcap_components(Components),
		NewTcapMsgDec = TcapMsgDec#'MapSpecificPDUs_begin'{dialoguePortion=NewDialg, components=NewComponents};
	{'continue', #'MapSpecificPDUs_continue'{dialoguePortion=Dialg, components=Components}} ->
		NewDialg = handle_tcap_dialogue(Dialg),
		NewComponents = handle_tcap_components(Components),
		NewTcapMsgDec = TcapMsgDec#'MapSpecificPDUs_continue'{dialoguePortion=NewDialg, components=NewComponents};
	{'end', #'MapSpecificPDUs_end'{dialoguePortion=Dialg, components=Components}} ->
		NewDialg = handle_tcap_dialogue(Dialg),
		NewComponents = handle_tcap_components(Components),
		NewTcapMsgDec = TcapMsgDec#'MapSpecificPDUs_end'{dialoguePortion=NewDialg, components=NewComponents};
	%{_, #'Abort'{reason=Reason} ->
	_ ->
		NewTcapMsgDec = TcapMsgDec
	end,
	io:format("new TcapMsgDec (Type=~p) ~p~n", [Type, NewTcapMsgDec]),
	{Type, NewTcapMsgDec}.

