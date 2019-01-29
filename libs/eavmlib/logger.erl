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

%%-----------------------------------------------------------------------------
%% @doc A simple logging module.
%%
%% This module can be used to log messages to the console.
%%
%% Most applications should use the logging macros defined in logger.hrl
%% to do logging.
%% @end
%%-----------------------------------------------------------------------------
-module(logger).

-export([start/0, start/1, log/3, stop/0]).
-export([loop/1]).

-record(state, {
    pid,
    config
}).

-type location() :: {
    Module::module(), Function::atom(), Arity::non_neg_integer(), Line::non_neg_integer()
}.
-type level() :: debug | info | warning | error.
-type config_item() :: {levels, [level()]}.
-type config() :: [config_item()].

%%-----------------------------------------------------------------------------
%% @equiv   start([{levels, [info, warning, error]}])
%% @doc     Start the logger.
%% @end
%%-----------------------------------------------------------------------------
-spec start() -> {ok, pid()}.
start() ->
    start([{levels, [info, warning, error]}]).

%%-----------------------------------------------------------------------------
%% @param   Config the logging configuration
%% @returns {ok, Pid}
%% @doc     Start the logger.
%%
%%          This function will start the logger with the specified configuration.
%% @end
%%-----------------------------------------------------------------------------
-spec start(Config::config()) -> {ok, pid()}.
start(Config) ->
    LoggerPid = case whereis(?MODULE) of
        undefined ->
            State = #state{pid=self(), config=Config},
            Pid = spawn(?MODULE, loop, [State]),
            receive
                started -> ok
            end,
            erlang:register(?MODULE, Pid),
            Pid;
        Pid -> Pid
    end,
    {ok, LoggerPid}.

%%-----------------------------------------------------------------------------
%% @doc     Stop the logger.
%%
%%          This function is rarely used.
%% @end
%%-----------------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    {ok, Pid} = maybe_start(whereis(?MODULE)),
    Pid ! stop,
    ok.

%%-----------------------------------------------------------------------------
%% @param   Location the location in the source module
%% @doc     Log a message the the specified location with the specified level.
%%
%%          Users should use the logging macros in logger.hrl instead of
%%          calling this function directly.
%% @end
%%-----------------------------------------------------------------------------
-spec log(Location::location(), Level::level(), Msg::term()) -> ok.
log(Location, Level, Msg) ->
    {ok, Pid} = maybe_start(whereis(?MODULE)),
    Pid ! {Location, erlang:universaltime(), self(), Level, Msg},
    ok.

%%
%% Internal operations
%%

%% @private
loop(#state{pid=Pid, config=Config} = State0) ->
    State = case Pid of
        undefined -> State0;
        _ ->
            Pid ! started,
            State0#state{pid=undefined}
    end,
    receive
        {_Location, _Time, _Pid, Level, _Msg} = Request ->
            do_log(Request, Level, proplists:get_value(levels, Config)),
            loop(State);
        stop ->
            ok;
        _ ->
            loop(State)
    end.

%% @private
maybe_start(undefined) ->
    start();
maybe_start(Pid) ->
    {ok, Pid}.

%% @private
do_log(_Request, _Level, undefined) ->
    ok;
do_log(Request, Level, Levels) ->
    case lists:member(Level, Levels) of
        true ->
            erlang:display(Request);
        _ ->
            ok
    end.
