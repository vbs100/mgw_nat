% SCTP Handler for gateway between MSC and STP, transparently
% rewriting addresses on the fly

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

-module(sctp_handler).
-author("Harald Welte <laforge@gnumonks.org>").
-export([init/2, handle_sctp/2, sctp_local_out/4]).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/inet_sctp.hrl").

%-include("m2ua.hrl").
%-include("mtp3.hrl").
%-include("isup.hrl").
%-include("sccp.hrl").

-record(loop_data,
	{msc_sock, msc_local_ip, msc_remote_ip, msc_remote_port,
	 msc_local_port, msc_assoc_id, 
	 stp_sock, stp_remote_ip, stp_remote_port, stp_assoc_id,
	 rewrite_act_mod
	}).

-define(COMMON_SOCKOPTS, [{active, once}, {reuseaddr, true}, {sctp_nodelay, true}]).

opts2local_stp(OptList) ->
	opts2local_stp(OptList, []).
opts2local_stp([], L) ->
	L;
opts2local_stp([{stp, {{Ip, Port}, _}}|Tail], L) ->
	opts2local_stp(Tail, [{ip, Ip}, {port, Port}] ++ L);
opts2local_stp([_|Tail], L) ->
	opts2local_stp(Tail, L).

% initialize the sockets towards MSC (listening) and STP (connect)
init(Options, RewriteActMod) when is_list(Options) ->
	{{MscLocalIP, MscLocalPort}, MscRemoteIP} = proplists:get_value(msc, Options),
	{_, {StpRemoteIP, StpRemotePort}} = proplists:get_value(stp, Options),
	{ok, MscSock} = gen_sctp:open([{ip, MscLocalIP},{port,MscLocalPort}]
					++ ?COMMON_SOCKOPTS),
	io:format("Listening for MSC on ~w:~w. ~w~n",
			[MscLocalIP, MscLocalPort, MscSock]),
	ok = gen_sctp:listen(MscSock, true),
	LocalSockOpts = opts2local_stp(Options),
	{ok, StpSock} = gen_sctp:open(LocalSockOpts ++ ?COMMON_SOCKOPTS),
	L = #loop_data{msc_sock = MscSock, msc_local_ip = MscLocalIP, 
			msc_remote_ip = MscRemoteIP,
			stp_sock = StpSock, stp_remote_ip = StpRemoteIP,
			stp_remote_port = StpRemotePort, rewrite_act_mod = RewriteActMod},
	{ok, L}.

% initiate a connection to STP as a client
initiate_stp_connection(#loop_data{stp_sock = Sock, stp_remote_ip = IP, stp_remote_port = Port}, Opts) ->
	io:format("Establishing SCTP conn to STP ~p Port ~p~n", [IP, Port]),
	gen_sctp:connect(Sock, IP, Port, Opts ++ ?COMMON_SOCKOPTS).

% main loop function
handle_sctp(L = #loop_data{msc_sock=MscSock, msc_remote_ip=MscRemoteIp, msc_remote_port=MscRemotePort,
		    stp_sock=StpSock, stp_remote_ip=StpRemoteIp, stp_remote_port=StpRemotePort,
		    rewrite_act_mod=RewriteActMod},
	    Sctp) ->
	%io:format("Entering receive loop ~p~n", [L]),
	%io:format("======================================================================~n"),
	case Sctp of
		% MSC connect or disconnect
		{sctp, MscSock, MscRemoteIp, Port, {ANC, SAC}}
					when is_record(SAC, sctp_assoc_change) ->
			io:format("MSC sctp_assoc_change ~p ~p~n", [ANC, SAC]),
			#sctp_assoc_change{state = SacState, outbound_streams = OutStreams,
					   inbound_streams = InStreams, assoc_id = MscAssocId} = SAC,
			case SacState of
				comm_up ->
					InitMsg = #sctp_initmsg{num_ostreams=InStreams,
								max_instreams=OutStreams},
					{ok, StpAssoc} = initiate_stp_connection(L, [{sctp_initmsg,InitMsg}]),
					io:format("STP Assoc: ~p~n", [StpAssoc]),
					NewL = L#loop_data{msc_remote_port = Port,
						 msc_assoc_id = MscAssocId,
						 stp_assoc_id = StpAssoc#sctp_assoc_change.assoc_id};
				comm_lost ->
					NewL = L,
					% maybe we should simply die?
					io:format("MSC SCTP comm_lost~n"),
					exit(sctp_comm_lost_msc);
				addr_unreachable ->
					NewL = L,
					io:format("MSC SCTP addr_unreachable~n"),
					% maybe we should simply die?
					exit(sctp_addr_unreach_msc)
			end,
			inet:setopts(MscSock, [{active, once}]);
		% STP connect or disconnect
		{sctp, StpSock, StpRemoteIp, StpRemotePort, {_Anc, SAC}}
					when is_record(SAC, sctp_assoc_change) ->
			io:format("STP sctp_assoc_change ~p~n", [SAC]),
			inet:setopts(StpSock, [{active, once}]),
			NewL = L;
		% MSC data
		{sctp, MscSock, MscRemoteIp, MscRemotePort, {[Anc], Data}} ->
			%io:format("MSC rx data: ~p ~p~n", [Anc, Data]),
			handle_rx_data(RewriteActMod, L, from_msc, Anc, Data),
			inet:setopts(MscSock, [{active, once}]),
			NewL = L;
		% STP data
		{sctp, StpSock, StpRemoteIp, StpRemotePort, {[Anc], Data}} ->
			%io:format("STP rx data: ~p ~p~n", [Anc, Data]),
			handle_rx_data(RewriteActMod, L, from_stp, Anc, Data),
			inet:setopts(StpSock, [{active, once}]),
			NewL = L;
		{sctp, _Sock, RemoteIp, _Remote_port, {_Anc, Data}}
					when is_record(Data, sctp_shutdown_event) ->
			% maybe we should simply die?
			NewL = L,
			io:format("SCTP remote ~p shutdown~n", [RemoteIp]),
			exit(sctp_remote_shutdown);
		{sctp, _Sock, RemoteIp, _RemotePort, {_Anc, Data}}
					when is_record(Data, sctp_paddr_change) ->
			case Data#sctp_paddr_change.state of
				addr_unreachable ->
					io:format("SCTP remote ~p unreachable~n",
						  [RemoteIp]),
					exit(sctp_addr_unreachable);
				_ ->
					ok
			end,
			NewL = L;
		Other ->
			io:format("OTHER ~p~n", [Other]),
			NewL = L
	end,
	NewL.


% handle incoming data on one of the SCTP sockets
handle_rx_data(Mod, L, From, SRInf = #sctp_sndrcvinfo{ppid = 2,
						 stream = Stream}, Data) when is_binary(Data) ->
	try Mod:rewrite_actor(sctp, From, [L, SRInf], 2, Data) of
		DataOut ->
			% send mangled data to other peer
			case From of
				from_msc ->
					Sock = L#loop_data.stp_sock,
					AssocId = L#loop_data.stp_assoc_id;
				from_stp ->
					Sock = L#loop_data.msc_sock,
					AssocId = L#loop_data.msc_assoc_id
			end,
			SndRcvInfo = #sctp_sndrcvinfo{ppid = 2, stream = Stream, assoc_id = AssocId},
			%io:format("Sending ~p to ~p ~p~n", [DataOut, Sock, SndRcvInfo]),
			% if they are not equal, we will abort here
			if DataOut == Data ->
				ok;
			   true ->
				io:format("Data is NOT equal~n")
			end,
			ok = gen_sctp:send(Sock, SndRcvInfo, DataOut)
	catch
		throw:stolen ->
			io:format("A message was stolen, not forwarding it~n")
	end;


handle_rx_data(_Mod, _L, From, SRInfo, Data) when is_binary(Data) ->
	io:format("Unhandled Rx Data from SCTP from ~p: ~p, ~p~n", [From, SRInfo, Data]).


sctp_local_out(L, From, OutData, SRInf) when is_record(L, loop_data),
					     is_record(SRInf, sctp_sndrcvinfo) ->
	case From of
		from_msc ->
			Sock = L#loop_data.stp_sock,
			AssocId = L#loop_data.stp_assoc_id;
		from_stp ->
			Sock = L#loop_data.msc_sock,
			AssocId = L#loop_data.msc_assoc_id
	end,
	gen_sctp:send(Sock, SRInf#sctp_sndrcvinfo{assoc_id = AssocId}, OutData).
