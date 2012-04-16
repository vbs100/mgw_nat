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
%
% Additional Permission under GNU AGPL version 3 section 7:
%
% If you modify this Program, or any covered work, by linking or
% combining it with runtime libraries of Erlang/OTP as released by
% Ericsson on http://www.erlang.org (or a modified version of these
% libraries), containing parts covered by the terms of the Erlang Public
% License (http://www.erlang.org/EPLICENSE), the licensors of this
% Program grant you additional permission to convey the resulting work
% without the need to license the runtime libraries of Erlang/OTP under
% the GNU Affero General Public License. Corresponding Source for a
% non-source form of such a combination shall include the source code
% for the parts of the runtime libraries of Erlang/OTP used as well as
% that of the covered work.

-module(mgw_nat_usr).
-author("Harald Welte <laforge@gnumonks.org>").

-behavior(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, handle_cast/2, handle_info/2, terminate/2]).


start_link(Params) ->
	MscName = get_cfg_pl_val(msc_name, Params),
	gen_server:start_link({local, MscName}, ?MODULE, Params, []).

stop() ->
	gen_server:cast(?MODULE, stop).

%% Callback functions of the OTP behavior

init(Params) ->
	io:format("Starting mgw_nat_usr with Args ~p~n", [Params]),
	MscLocalIp = get_cfg_pl_val(msc_local_ip, Params),
	MscLocalPort = get_cfg_pl_val(msc_local_port, Params),
	MscRemoteIp = get_cfg_pl_val(msc_remote_ip, Params),
	StpRemoteIp = get_cfg_pl_val(stp_remote_ip, Params),
	StpRemotePort = get_cfg_pl_val(stp_remote_port, Params),
	RewriteActMod = get_cfg_pl_val(rewrite_act_mod, Params),
	RewriteActMod:reload_config(),
	SctpHdlrArgs =	[MscLocalIp, MscLocalPort, MscRemoteIp,
			 StpRemoteIp, StpRemotePort, RewriteActMod],
	{ok, LoopDat} = apply(sctp_handler, init, SctpHdlrArgs),
	{ok, {Params, LoopDat}}.

% this cast is produced by mgw_nat_sup child walker
handle_cast(reload_config, L = {Params, _LoopData}) ->
	RewriteActMod = get_cfg_pl_val(rewrite_act_mod, Params),
	RewriteActMod:reload_config(),
	{noreply, L};

handle_cast(stop, LoopData) ->
	{stop, normal, LoopData}.


terminate(_Reason, _LoopData) ->
	ok.

% callback for other events like incoming SCTP message
handle_info({sctp, Sock, Ip, Port, Data}, {InitParams, LoopData}) ->
	NewL = sctp_handler:handle_sctp(LoopData, {sctp, Sock, Ip, Port, Data}),
	{noreply, {InitParams, NewL}}.

% wrapper around proplists:get_value() to check for missing stuff
get_cfg_pl_val(Name, List) ->
	case proplists:get_value(Name, List) of
	    undefined ->
		error_logger:error_report([{error, app_cfg_missing},
					   {get_cfg_pl_val, Name}]);
	    Val ->
		Val
	end.
