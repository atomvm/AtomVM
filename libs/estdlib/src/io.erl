%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2019 by Fred Dushin <fred@dushin.net>                       %
%                                                                         %
%   This program is free software; you can redistribute it and/or modify  %
%   it under the terms of the GNU Lesser General Public License as        %
%   published by the Free Software Foundation; either version 2 of the    %
%   License, or (at your option) any later version.                       %
%                                                                         %
%   This program is distributed in the hope that it will be useful,       %
%   but WITHOUT ANY WARRANTY; without even the implied warranty of        %
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         %
%   GNU General Public License for more details.                          %
%                                                                         %
%   You should have received a copy of the GNU General Public License     %
%   along with this program; if not, write to the                         %
%   Free Software Foundation, Inc.,                                       %
%   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(io).

-export([format/1, format/2, get_line/1, put_chars/1]).

%%-----------------------------------------------------------------------------
%% @doc     Equivalent to format(Format, []).
%% @end
%%-----------------------------------------------------------------------------
-spec format(Format::string()) -> string().
format(Format) when is_list(Format) ->
    format(Format, []).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Args format argument
%% @returns string
%% @doc     Format string and data to console.
%%          See io_lib:format/2 for information about
%%          formatting capabilities.
%% @end
%%-----------------------------------------------------------------------------
-spec format(Format::string(), Args::list()) -> string().
format(Format, Args) when is_list(Format) andalso is_list(Args) ->
    Msg =
        try
            io_lib:format(Format, Args)
        catch
            _:_ ->
                io_lib:format("Bad format!  Format: ~p Args: ~p~n", [Format, Args])
        end,
    put_chars(Msg).

get_line(Prompt) ->
    Self = self(),
    case erlang:group_leader() of
        Self ->
            erlang:throw(no_group_leader);
        Leader ->
            Ref = make_ref(),
            Leader ! {io_request, self(), Ref, {get_line, unicode, Prompt}},
            receive
                {io_reply, Ref, Line} -> Line
            end
    end.

put_chars(Chars) ->
    Self = self(),
    case erlang:group_leader() of
        Self -> console:print(Chars);
        Leader -> Leader ! {io_request, self(), make_ref(), {put_chars, unicode, Chars}}
    end.
