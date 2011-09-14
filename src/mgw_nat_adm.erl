% Administrative process for MGW NAT

% The administrative process takes care of re-loading the configuration
% after it has been re-parsed.  This includes delivering a reload_config
% signal to all child processes of the supervisor.  We don't do this
% inside the supervisor itself, as there might be an exception.

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

-module(mgw_nat_adm).
-author("Harald Welte <laforge@gnumonks.org>").

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_info/2, terminate/2, start_link/1]).
-export([sccp_masq_reset/0, sccp_masq_dump/0, reload_config/0]).


sccp_masq_reset() ->
	gen_server:cast(?MODULE, sccp_masq_reset).

sccp_masq_dump() ->
	gen_server:cast(?MODULE, sccp_masq_dump).

reload_config() ->
	gen_server:cast(?MODULE, reload_all_config).


start_link(Params) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

init(_Params) ->
	{ok, foo}.

handle_cast(sccp_masq_reset, LoopData) ->
	sccp_masq:reset(),
	{noreply, LoopData};

handle_cast(sccp_masq_dump, LoopData) ->
	sccp_masq:dump(),
	{noreply, LoopData};

handle_cast(reload_config, LoopData) ->
	{noreply, LoopData};

handle_cast(reload_all_config, LoopData) ->
	map_masq:config_update(),
	% now we iterate over the children and deliver the signal
	Children = supervisor:which_children(mgw_nat_sup),
	cast_to_children(Children, reload_config),
	% and finally return to the main loop
	{noreply, LoopData}.

handle_info(Info, LoopData) ->
	{noreply, LoopData}.

terminate(_Reason, _LoopData) ->
	ok.

cast_to_children([], _Cast) ->
	ok;
cast_to_children([Child|Tail], Cast) ->
	{Name, Pid, _Type, _Modules} = Child,
	io:format("Casting ~p to ~p(~p)~n", [Cast, Name, Pid]),
	gen_server:cast(Pid, Cast),
	cast_to_children(Tail, Cast).
