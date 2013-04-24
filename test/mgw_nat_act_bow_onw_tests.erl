-module(mgw_nat_act_bow_onw_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").

-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/isup.hrl").
-include_lib("osmo_ss7/include/osmo_util.hrl").

% unmodified original called party number in national format
-define(IAM_CALLED_DEC_IN,
	#party_number{nature_of_addr_ind=?ISUP_ADDR_NAT_NATIONAL,
		      internal_net_num=1,
		      number_incompl_ind=undefined,
		      numbering_plan=1,
		      present_restrict=undefined,
		      screening_ind=undefined,
		      phone_number=[9,2,9,9,4,1,9,0,8,7,15]}).

-define(IAM_CALLING_EMPTY_DEC_IN,
	#party_number{nature_of_addr_ind=?ISUP_ADDR_NAT_NATIONAL,
		      internal_net_num=undefined,
		      number_incompl_ind=0,
		      numbering_plan=1,
		      present_restrict=1,
		      screening_ind=3,
		      phone_number=[]}).

% modified called party number in international format
-define(IAM_CALLED_DEC_OUT,
	?IAM_CALLED_DEC_IN#party_number{nature_of_addr_ind=?ISUP_ADDR_NAT_INTERNATIONAL,
					phone_number=[4,9,9,2,9,9,4,1,9,0,8,7,15]}).



% IAM message in STP-MSC direction containing national number
-define(ISUP_IAM_DEC, {isup_msg,1,14, [{conn_ind_nature,16}, {fw_call_ind,24832},
				      {calling_cat,10}, {transm_medium_req,3},
				      {?ISUP_PAR_CALLED_P_NUM, ?IAM_CALLED_DEC_IN},
				      {?ISUP_PAR_CALLING_P_NUM, ?IAM_CALLING_EMPTY_DEC_IN},
				      {57,{2,<<"1À">>}}, {29,{3,<<144,144,163>>}}]}).

% modified IAM message in STP-MSC direction containing internationalized number
-define(ISUP_IAM_OUT, {isup_msg,1,14, [{conn_ind_nature,16}, {fw_call_ind,24832},
				      {calling_cat,10}, {transm_medium_req,3},
				      {?ISUP_PAR_CALLED_P_NUM, ?IAM_CALLED_DEC_OUT},
				      {?ISUP_PAR_CALLING_P_NUM, ?IAM_CALLING_EMPTY_DEC_IN},
				      {57,{2,<<"1À">>}}, {29,{3,<<144,144,163>>}}]}).


% variant: calling number is national number with 00 prefix, need to
% remove 00 and set to international
-define(IAM_CALLING00_DEC_IN,
	?IAM_CALLING_EMPTY_DEC_IN#party_number{phone_number=[0,0,4,9,1,2,3,4,5,6,7,8,9,0,15]}).
-define(IAM_CALLING00_DEC_OUT,
	?IAM_CALLING00_DEC_IN#party_number{nature_of_addr_ind=?ISUP_ADDR_NAT_INTERNATIONAL,
					   phone_number=[4,9,1,2,3,4,5,6,7,8,9,0,15]}).
-define(ISUP_IAM00_DEC, {isup_msg,1,14, [{conn_ind_nature,16}, {fw_call_ind,24832},
				      {calling_cat,10}, {transm_medium_req,3},
				      {?ISUP_PAR_CALLED_P_NUM, ?IAM_CALLED_DEC_IN},
				      {?ISUP_PAR_CALLING_P_NUM, ?IAM_CALLING00_DEC_IN},
				      {57,{2,<<"1À">>}}, {29,{3,<<144,144,163>>}}]}).
-define(ISUP_IAM00_OUT, {isup_msg,1,14, [{conn_ind_nature,16}, {fw_call_ind,24832},
				      {calling_cat,10}, {transm_medium_req,3},
				      {?ISUP_PAR_CALLED_P_NUM, ?IAM_CALLED_DEC_OUT},
				      {?ISUP_PAR_CALLING_P_NUM, ?IAM_CALLING00_DEC_OUT},
				      {57,{2,<<"1À">>}}, {29,{3,<<144,144,163>>}}]}).

setup() ->
	application:set_env(mgw_nat, msrn_pfx_msc, 41489099),
	application:set_env(mgw_nat, msrn_pfx_stp, 4991806040),
	application:set_env(mgw_nat, intern_pfx, 49).
	%mgw_nat_act_vfuk_onw:reload_config().

teardown(_) ->
	application:unset_env(mgw_nat, msrn_pfx_msc),
	application:unset_env(mgw_nat, msrn_pfx_stp),
	application:unset_env(mgw_nat, intenr_pfx).


mangle_rx_iam() ->
	?assertEqual(?ISUP_IAM_OUT, mgw_nat:mangle_rx_isup(from_stp, path, 1, ?ISUP_IAM_DEC)).

mangle_rx_iam00() ->
	?assertEqual(?ISUP_IAM00_OUT, mgw_nat:mangle_rx_isup(from_stp, path, 1, ?ISUP_IAM00_DEC)).

bow_onw_test_() ->
	{setup,
		fun setup/0,
		fun teardown/1,
		[ ?_test(mangle_rx_iam()),
	          ?_test(mangle_rx_iam00())
		]
	}.
