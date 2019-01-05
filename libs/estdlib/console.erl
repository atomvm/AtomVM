%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2018 by Fred Dushin <fred@dushin.net>                       %
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
-module(console).

-export([start/0, puts/1, puts/2, flush/0, flush/1]).

-opaque console() :: any().

-spec puts(string()) -> ok.
puts(String) ->
    puts(get_pid(), String).

%% @hidden
-spec puts(console(), string()) -> ok.
puts(Console, String) ->
    call(Console, {puts, String}).

-spec flush() -> ok.
flush() ->
    flush(get_pid()).

%% @hidden
-spec flush(console()) -> ok.
flush(Console) ->
    call(Console, flush).

%% Internal operations

%% @private
-spec call(console(), string()) -> ok.
call(Console, Msg) ->
    Ref = make_ref(),
    Console ! {self(), Ref, Msg},
    receive
        {Ref, Reply} -> Reply
    end.

%% @private
-spec get_pid() -> console().
get_pid() ->
    case whereis(console) of
        undefined ->
            start();
        Pid when is_pid(Pid) ->
            Pid
    end.

%% @private
-spec start() -> console().
start() ->
    Pid = erlang:open_port({spawn, "console"}, []),
    erlang:register(console, Pid),
    Pid.
