% BOW-ONW specific mgw_nat actor callback functions

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


-module(mgw_nat_act_bow_onw).
-author("Harald Welte <laforge@gnumonks.org>").

-export([rewrite_actor/5, reload_config/0]).

-include_lib("osmo_ss7/include/sccp.hrl").

% Rewrite at SCTP (root) level:
rewrite_actor(sctp, From, Path, 2, DataBin) ->
	mgw_nat:mangle_rx_data(From, Path, DataBin, fun rewrite_actor/5);

% Rewrite at ISUP level:
rewrite_actor(isup, From, Path, MsgType, IsupDec) ->
	mgw_nat:mangle_rx_isup(From, Path, MsgType, IsupDec);

% Rewrite at SCCP level: Static GT rewrite as well as dynamic masquerading
rewrite_actor(sccp, From, Path, MsgType, SccpDec) ->
	SccpMangled = mgw_nat:mangle_rx_sccp(From, Path, MsgType, SccpDec),
	SccpMasqued = sccp_masq:sccp_masq_msg(From, SccpMangled#sccp_msg.msg_type, SccpMangled),
	SccpMasqued;

% Rewrite at MAP level: call into map_masq module
rewrite_actor(map, From, _Path, 0, MapDec) ->
	map_masq:mangle_map(From, MapDec);

% Default action: no rewrite
rewrite_actor(_Level, _From, _Path, _MsgType, Msg) ->
	Msg.

reload_config() ->
	ok.
