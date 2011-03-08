% Wrapper code, wrapping sctp_handler.erl into OTP gen_server

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

-module(mgw_nat_usr).
-author("Harald Welte <laforge@gnumonks.org>").

-behavior(gen_server).

-export([start_link/1, stop/0, sccp_masq_reset/0, sccp_masq_dump/0]).
-export([init/1, handle_cast/2, handle_info/2, terminate/2]).


start_link(Params) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

stop() ->
	gen_server:cast(?MODULE, stop).

sccp_masq_reset() ->
	gen_server:cast(?MODULE, sccp_masq_reset).

sccp_masq_dump() ->
	gen_server:cast(?MODULE, sccp_masq_dump).


%% Callback functions of the OTP behavior

init(_Params) ->
	sccp_masq:init(),
	map_masq:config_update(),
	{ok, MscLocalIp} = application:get_env(msc_local_ip),
	{ok, MscLocalPort} = application:get_env(msc_local_port),
	{ok, MscRemoteIp} = application:get_env(msc_remote_ip),
	{ok, StpRemoteIp} = application:get_env(stp_remote_ip),
	{ok, StpRemotePort} = application:get_env(stp_remote_port),
	{ok, RewriteActor} = application:get_env(rewrite_actor),
	HandleFn = get_handle_fn(RewriteActor),
	io:format("Starting mgw_nat_usr with rewrite actor ~p~n", [RewriteActor]),
	SctpHdlrArgs =	[MscLocalIp, MscLocalPort, MscRemoteIp,
			 StpRemoteIp, StpRemotePort, HandleFn],
	apply(sctp_handler, init, SctpHdlrArgs).

handle_cast(stop, LoopData) ->
	{stop, normal, LoopData};

handle_cast(sccp_masq_reset, LoopData) ->
	sccp_masq:reset(),
	{noreply, LoopData};

handle_cast(sccp_masq_dump, LoopData) ->
	sccp_masq:dump(),
	{noreply, LoopData}.

terminate(_Reason, _LoopData) ->
	ok.

% callback for other events like incoming SCTP message
handle_info({sctp, Sock, Ip, Port, Data}, LoopData) ->
	NewL = sctp_handler:handle_sctp(LoopData, {sctp, Sock, Ip, Port, Data}),
	{noreply, NewL}.


% return rewrite_actor function reference
get_handle_fn(bow_onw) ->
	fun mgw_nat_act_bow_onw:rewrite_actor/5;
get_handle_fn(vfuk_onw) ->
	fun mgw_nat_act_vfuk_onw:rewrite_actor/5.
