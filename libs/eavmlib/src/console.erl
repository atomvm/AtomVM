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

%%-----------------------------------------------------------------------------
%% @doc This modules supports output of string data to the console.
%% @end
%%-----------------------------------------------------------------------------
-module(console).

-export([start/0, puts/1, puts/2, flush/0, flush/1, print/1]).

%%-----------------------------------------------------------------------------
%% @param   String the string data to write to the console
%% @returns ok if the data was written, or {error, Reason}, if there was
%%          an error.
%% @see     erlang:display/1
%% @doc     Write a string to the console.
%%
%%          <em><b>Note.</b>  This operation will only write string data.
%%          The output is not suffixed with a newline character or sequence.
%%          To print an erlang term, use erlang:display/1.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec puts(string()) -> ok | {error, term()}.
puts(String) ->
    puts(get_pid(), String).

%% @hidden
-spec puts(pid(), string()) -> ok.
puts(Console, String) ->
    call(Console, {puts, String}).

%%-----------------------------------------------------------------------------
%% @returns ok if the data was written, or {error, Reason}, if there was
%%          an error.
%% @doc     Flush any previously written data to the console.
%% @end
%%-----------------------------------------------------------------------------
-spec flush() -> ok.
flush() ->
    flush(get_pid()).

%% @hidden
-spec flush(pid()) -> ok.
flush(Console) ->
    call(Console, flush).
    
%%-----------------------------------------------------------------------------
%% @param   String the string data to write to the console
%% @returns ok if the data was written, or {error, Reason}, if there was
%%          an error.
%% @see     erlang:display/1
%% @doc     Write a string to the console.
%% @end
%%-----------------------------------------------------------------------------
-spec print(string()) -> ok | error.
print(String) ->
    throw(nif_error).


%% Internal operations

%% @private
-spec call(pid(), string()) -> ok.
call(Console, Msg) ->
    Ref = make_ref(),
    Console ! {self(), Ref, Msg},
    receive
        {Ref, Reply} -> Reply
    end.

%% @private
-spec get_pid() -> pid().
get_pid() ->
    case whereis(console) of
        undefined ->
            start();
        Pid when is_pid(Pid) ->
            Pid
    end.

%% @private
-spec start() -> pid().
start() ->
    Pid = erlang:open_port({spawn, "console"}, []),
    erlang:register(console, Pid),
    Pid.
