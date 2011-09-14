% OTP Supervisor for MGW NAT

% (C) 2011 by Harald Welte <laforge@gnumonks.org>
% (C) 2011 OnWaves
%
% All Rights Reserved
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as
% published by the Free Software Foundation; either version 3 of the
% License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU Affero General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(mgw_nat_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
	sccp_masq:init(),
	map_masq:config_update(),
	SignLinkList = get_app_config(sign_links),
	ChildList = gen_child_list(SignLinkList),
	AdmChild = {mgw_nat_adm, {mgw_nat_adm, start_link, [foo]},
		    permanent, 2000, worker, [mgw_nat_usr, sctp_handler,
					      mgw_nat, mgw_nat_adm]},
	{ok,{{one_for_one,60,600}, [AdmChild|ChildList]}}.

% generate a list of child specifications, one for each signalling link
gen_child_list(SignLinkList) ->
	gen_child_list(SignLinkList, []).
gen_child_list([], ChildList) ->
	ChildList;
gen_child_list([Link|Tail], ChildList) ->
	{Name, ChildArgs} = Link,
	NewChild = {Name, {mgw_nat_usr, start_link, [[{msc_name, Name}|ChildArgs]]},
		    permanent, 2000, worker, [mgw_nat_usr, sctp_handler, mgw_nat]},
	gen_child_list(Tail, [NewChild|ChildList]).

get_app_config(Name) ->
	case application:get_env(Name) of
	    undefined ->
		error_logger:error_report([{error, app_cfg_missing},
					   {get_app_config, Name}]),
		throw(app_cfg_missing);
	    {ok, Val} ->
		Val
	end.
