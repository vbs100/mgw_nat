% Maintain a list of IMSIs in a gb_tree and match against it

% (C) 2012-2013 by Harald Welte <laforge@gnumonks.org>
% (C) 2012-2013 by On-Waves
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

-module(imsi_list).
-author('Harald Welte <laforge@gnumonks.org>').

-export([read_file/1, read_list/1, match_imsi/2, match_imsi/3]).

-record(state, {forward, reverse}).

lines2tree(Iodev) ->
	S = #state{forward = gb_trees:empty(),
		   reverse = gb_trees:empty()},
	lines2tree(Iodev, S).

chomp(Line) when is_list(Line) ->
	case lists:last(Line) of
		10 ->
			lists:sublist(Line, 1, length(Line)-1);
		_ ->
			Line
	end.

% convert from "12345" to [1,2,3,4,5]
string_num_to_int_list(Line2) ->
	[case string:to_integer([X]) of
		{Int,[]} -> Int;
		{error, F} ->
			error_logger:error_report([{imsi_list_syntax_error,
						Line2, {error, F}}]),
			undefined
	 end || X <- Line2].

lines2tree(Iodev, State) ->
	case file:read_line(Iodev) of
		eof ->
			{ok, State};
		{error, Reason} ->
			{error, Reason};
		ebadf ->
			{error, ebadf};
		{ok, Line} ->
			% FIXME: convert to digit list
			Line2 = chomp(Line),
			case string:tokens(Line2, ",;") of
				[ImsiOldStr, ImsiNewStr] ->
					ImsiOld = string_num_to_int_list(ImsiOldStr),
					ImsiNew = string_num_to_int_list(ImsiNewStr),
					FwNew = gb_trees:insert(ImsiOld, ImsiNew,
								State#state.forward),
					RevNew = gb_trees:insert(ImsiNew, ImsiOld,
								 State#state.reverse),
					lines2tree(Iodev, #state{forward = FwNew,
								 reverse = RevNew});
				% FIXME: handle empty lines or skip bad lines
				_ ->
					{error, file_format}
			end
	end.


read_file(FileName) ->
	% read a text file with one IMSI per line into a gb_tree
	case file:open(FileName, [read]) of
		{ok, IoDev} ->
			lines2tree(IoDev);
		{error, Reason} ->
			{error, Reason}
	end.

read_list(List) when is_list(List) ->
	S = #state{forward = gb_trees:empty(),
		   reverse = gb_trees:empty()},
	read_list(List, S).

read_list([], Tree) ->
	Tree;
read_list([{Old, New}|Tail], State) ->
	FwNew = gb_trees:insert(Old, New, State#state.forward),
	RevNew = gb_trees:insert(New, Old, State#state.reverse),
	read_list(Tail, #state{forward = FwNew, reverse = RevNew}).

match_imsi(State, Imsi) when is_list(Imsi) ->
	match_imsi(forward, State, Imsi).

match_imsi(forward, State, Imsi) when is_list(Imsi) ->
	case gb_trees:lookup(Imsi, State#state.forward) of
		{value, ImsiNew} ->
			{ok, ImsiNew};
		none ->
			{error, no_entry}
	end;
match_imsi(reverse, State, Imsi) when is_list(Imsi) ->
	case gb_trees:lookup(Imsi, State#state.reverse) of
		{value, ImsiNew} ->
			{ok, ImsiNew};
		none ->
			{error, no_entry}
	end.
