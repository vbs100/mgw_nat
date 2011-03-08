% VFUK-ONW specific mgw_nat actor callback functions

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

-module(mgw_nat_act_vfuk_onw).
-author("Harald Welte <laforge@gnumonks.org>").

-export([rewrite_actor/5]).

% Rewrite at SCTP (root) level:
rewrite_actor(sctp, From, Path, 2, DataBin) ->
	try mgw_nat:mangle_rx_data(From, Path, DataBin, fun rewrite_actor/5) of
		Val ->
			Val
	catch error:Error ->
		% some parser error, simply forward msg unmodified
		io:format("MGW NAT mangling Error: ~p~n", [Error]),
		DataBin
	end;

% Rewrite at MAP level: call into map_masq module
rewrite_actor(map, From, Path, 0, MapDec) ->
	mangle_map_camel_phase(From, Path, MapDec);

% Default action: no rewrite
rewrite_actor(_Level, _From, _Path, _MsgType, Msg) ->
	Msg.


mangle_map_camel_phase(From, Path, MapDec) ->
	MapDec.
