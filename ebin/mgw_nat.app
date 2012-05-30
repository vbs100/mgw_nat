{application, mgw_nat,
	[{description, "Media Gateway NAT"},
	 {vsn, "1"},
	 {modules, [mgw_nat_app, mgw_nat_sup, mgw_nat_usr, mgw_nat,
		    mgw_nat_adm, sccp_masq, map_masq, sctp_handler,
		    mgw_nat_act_bow_onw, mgw_nat_act_vfuk_onw, imsi_list,
		    mangle_tt_sri_sm]},
	 {registered, [mgw_nat_app]},
	 {mod, {mgw_nat_app, []}},
	 {applications, []},
	 {env, [

		% SCCP static rewrite rules
		{sccp_rewrite_tbl, [
			{ 12340000, 98760000, "HLR" },
			{ 12340001, 98760001, "VLR" }
		]},

		% Example SCCP source masquerading pool
		{sccp_masq_gt_base, 12340000},
		{sccp_masq_gt_max, 9999},

		% Example ISUP rewrite
		{msrn_pfx_msc, 35489099},
		{msrn_pfx_stp, 6392994200},
		{intern_pfx, 63},

		% Example SCTP / IP config
		{sign_links, [
			{mgw_nat_msc1, [
				{msc_local_ip, any},
				{msc_local_port, 2904},
				{msc_remote_ip, {172,16,1,81}},
				{stp_remote_ip, {172,16,249,20}},
				{stp_remote_port, 2904},
				% Specify the rewrite actor module
				{rewrite_act_mod, mgw_nat_act_bow_onw }
			]},
			{mgw_nat_msc2, [
				{msc_local_ip, any},
				{msc_local_port, 2905},
				{msc_remote_ip, {172,16,1,81}},
				%{stp_remote_ip, {172,16,249,20}},
				{stp_remote_port, 2905},
				% Specify the rewrite actor module
				{rewrite_act_mod, mgw_nat_act_bow_onw }
			]}
		]},

		% Example MAP rewrite table
		{map_rewrite_table, [
			{ msc, 1234500070, 678980004014 },
			{ hlr, 1234500073, 678980004012 },
			{ scf, 1234500061, 678980004022 },
			{ vlr, 1234500071, 678980004013 },
			{ smsCDA, 678980000105, 678990000465 }
		]}
	  ]}
]}.
