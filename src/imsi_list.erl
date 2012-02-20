% Maintain a list of IMSIs in a gb_tree and match against it

% (C) 2012 by Harald Welte <laforge@gnumonks.org>
% (C) 2012 by On-Waves
%
% All Rights Reserved
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License along
% with this program; if not, write to the Free Software Foundation, Inc.,
% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

-module(imsi_list).
-author('Harald Welte <laforge@gnumonks.org>').

-export([read_file/1, read_list/1, match_imsi/2]).

lines2tree(Iodev) ->
	lines2tree(Iodev, gb_trees:empty()).

chomp(Line) when is_list(Line) ->
	case lists:last(Line) of
		10 ->
			lists:sublist(Line, 1, length(Line)-1);
		_ ->
			Line
	end.

% convert from "12345" to [1,2,3,4,5]
string_num_to_int_list(Line2) ->
	[case string:to_integer([X]) of {Int,[]} -> Int end || X <- Line2].

lines2tree(Iodev, Tree) ->
	case file:read_line(Iodev) of
		eof ->
			{ok, Tree};
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
					lines2tree(Iodev, gb_trees:insert(ImsiOld, ImsiNew, Tree));
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
	read_list(List, gb_trees:empty()).

read_list([], Tree) ->
	Tree;
read_list([{Old, New}|Tail], Tree) ->
	read_list(Tail, gb_trees:enter(Old, New, Tree)).

match_imsi(Tree, Imsi) when is_list(Imsi) ->
	case gb_trees:lookup(Imsi, Tree) of
		{value, ImsiNew} ->
			{ok, ImsiNew};
		none ->
			{error, no_entry}
	end.
