%
% This file is part of AtomVM.
%
% Copyright 2019 Fred Dushin <fred@dushin.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

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

-export([
    start/0, start/1, log/3,
    get_levels/0, set_levels/1,
    get_filter/0, set_filter/1,
    get_sinks/0, set_sinks/1,
    stop/0
]).
-export([loop/1, console_log/1]).

-record(state, {
    pid,
    config
}).

-type location() :: {
    Module::module(), Function::atom(), Arity::non_neg_integer(), Line::non_neg_integer()
}.
-type level() :: debug | info | warning | error.
-type sink() :: {atom(), atom()}.
-type year() :: integer().
-type month() :: 1..12.
-type day() :: 1..31.
-type hour() :: 0..59.
-type minute() :: 0..59.
-type second() :: 0..59.
-type timestamp() :: {year(), month(), day(), hour(), minute(), second()}.
-type msg_format() :: {string(), list(term())}.
-type log_request() :: {location(), timestamp(), pid(), level(), msg_format()}.

-type config_item() :: {levels, [level()]}.
-type config() :: [config_item()].

-define(DEFAULT_CONFIG, [{levels, [info, warning, error]}, {filter, []}, {sinks, [{?MODULE, console_log}]}]).

%%-----------------------------------------------------------------------------
%% @equiv   start([{levels, [info, warning, error]}])
%% @doc     Start the logger.
%% @end
%%-----------------------------------------------------------------------------
-spec start() -> {ok, pid()}.
start() ->
    start([]).

%%-----------------------------------------------------------------------------
%% @param   Config the logging configuration
%% @returns {ok, Pid}
%% @doc     Start the logger.
%%
%%          This function will start the logger with the specified configuration.
%% @end
%%-----------------------------------------------------------------------------
-spec start(Config::config()) -> {ok, pid()}.
start(Config0) ->
    Config = fill_defaults(Config0, ?DEFAULT_CONFIG),
    LoggerPid = case whereis(?MODULE) of
        undefined ->
            State = #state{pid=self(), config=Config},
            Pid = spawn(?MODULE, loop, [State]),
            receive
                started ->
                    ok
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
-spec log(Location::location(), Level::level(), MsgFormat::{Format::string(), Args::list(term())}) -> ok.
log(Location, Level, MsgFormat) ->
    {ok, Pid} = maybe_start(whereis(?MODULE)),
    Pid ! {Location, erlang:universaltime(), self(), Level, MsgFormat},
    ok.

%%-----------------------------------------------------------------------------
%% @param   Levels the list of levels to set
%% @return  ok
%% @doc     Set the levels in the logger.
%% @end
%%-----------------------------------------------------------------------------
-spec set_levels(Levels::[level()]) -> ok.
set_levels(Levels) ->
    {ok, Pid} = maybe_start(whereis(?MODULE)),
    Pid ! {set_levels, Levels},
    ok.

%%-----------------------------------------------------------------------------
%% @return  the current list of levels set in the logger
%% @doc     Get the current levels in the logger.
%% @end
%%-----------------------------------------------------------------------------
-spec get_levels() -> {ok, Levels::[level()]} | {error, timeout}.
get_levels() ->
    {ok, Pid} = maybe_start(whereis(?MODULE)),
    Ref = erlang:make_ref(),
    Pid ! {get_levels, Ref, self()},
    receive
        {Ref, Levels} ->
            {ok, Levels}
    after 5000 ->
        {error, timeout}
    end.

%%-----------------------------------------------------------------------------
%% @param   Filter the filter to set
%% @return  ok
%% @doc     Set the filter in the logger.
%%          Currently, a filter is a set of module names.  If the filter is
%%          non-empty, then a log will be triggered only if the module
%%          being logged is in the filter list; otherwise, if the filter is
%%          empty, then the log will be triggered (if the level matches, as well)
%% @end
%%-----------------------------------------------------------------------------
-spec set_filter(Filter::[module()]) -> ok.
set_filter(Filter) ->
    {ok, Pid} = maybe_start(whereis(?MODULE)),
    Pid ! {set_filter, Filter},
    ok.

%%-----------------------------------------------------------------------------
%% @return  the current filter set in the logger
%% @doc     Get the current filter in the logger.
%% @end
%%-----------------------------------------------------------------------------
-spec get_filter() -> {ok, Filter::[module()]} | {error, timeout}.
get_filter() ->
    {ok, Pid} = maybe_start(whereis(?MODULE)),
    Ref = erlang:make_ref(),
    Pid ! {get_filter, Ref, self()},
    receive
        {Ref, Filter} ->
            {ok, Filter}
    after 5000 ->
        {error, timeout}
    end.

%%-----------------------------------------------------------------------------
%% @param   Sinks the list of sinks to set
%% @return  ok
%% @doc     Set the sinks in the logger.
%% @end
%%-----------------------------------------------------------------------------
-spec set_sinks(Sinks::[sink()]) -> ok.
set_sinks(Sinks) ->
    {ok, Pid} = maybe_start(whereis(?MODULE)),
    Pid ! {set_sinks, Sinks},
    ok.

%%-----------------------------------------------------------------------------
%% @return  the current list of sinks set in the logger
%% @doc     Get the current sinks in the logger.
%% @end
%%-----------------------------------------------------------------------------
-spec get_sinks() -> {ok, Sinks::[sink()]} | {error, timeout}.
get_sinks() ->
    {ok, Pid} = maybe_start(whereis(?MODULE)),
    Ref = erlang:make_ref(),
    Pid ! {get_sinks, Ref, self()},
    receive
        {Ref, Sinks} ->
            {ok, Sinks}
    after 5000 ->
        {error, timeout}
    end.

%% @hidden
-spec console_log(Request::log_request()) -> ok.
console_log(Request) ->
    {Location, Timestamp, Pid, Level, {Format, Args}} = Request,
    TimestampStr = make_timestamp(Timestamp),
    MFALStr = make_location(Location),
    PidLevelStr = io_lib:format("~p ~p", [Pid, Level]),
    MsgStr = io_lib:format(Format, Args),
    io:format(TimestampStr ++ " " ++ MFALStr ++ " " ++ PidLevelStr ++ ": " ++ MsgStr ++ "~n").

make_timestamp(Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = Timestamp,
    io:format("~p-~p-~pT~p:~p:~p.000", [
        Year, Month, Day, Hour, Minute, Second
    ]).

make_location(Location) ->
    {Module, Function, Arity, Line} = Location,
    io:format("[~p:~p/~p:~p]", [
        Module, Function, Arity, Line
    ]).

%%
%% Internal operations
%%

%% @private
loop(#state{pid=StartingPid, config=Config} = State0) ->
    State = case StartingPid of
        undefined -> State0;
        _ ->
            StartingPid ! started,
            State0#state{pid=undefined}
    end,
    Levels = proplists:get_value(levels, Config, []),
    Filter = proplists:get_value(filter, Config, []),
    Sinks   = proplists:get_value(sinks, Config, []),
    receive
        {get_levels, Ref, Pid} ->
            Pid ! {Ref, Levels},
            loop(State);
        {get_filter, Ref, Pid} ->
            Pid ! {Ref, Filter},
            loop(State);
        {get_sinks, Ref, Pid} ->
            Pid ! {Ref, Sinks},
            loop(State);
        {set_levels, NewLevels} ->
            loop(State#state{config=[{levels, NewLevels} | lists:keydelete(levels, 1, Config)]});
        {set_filter, NewFilter} ->
            loop(State#state{config=[{filter, NewFilter} | lists:keydelete(filter, 1, Config)]});
        {set_sinks, NewSinks} ->
            loop(State#state{config=[{sinks, NewSinks} | lists:keydelete(sinks, 1, Config)]});
        {_Location, _Time, _Pid, Level, _MsgFormat} = Request ->
            maybe_do_log(Sinks, Request, Level, Levels, Filter),
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
maybe_do_log(_Sinks, _Request, _Level, undefined, _Filter) ->
    ok;
maybe_do_log(Sinks, {Location, _Time, _Pid, _Level, _MsgFormat} = Request, Level, Levels, Filter) ->
    case match_filter(Location, Filter) of
        true ->
            do_log_sinks(Sinks, Request, Level, Levels);
        _ ->
            ok
    end.

%% @private
match_filter(_Location, []) ->
    true;
match_filter({Module, _Function, _Arity, _Line}, Filter) ->
    lists:member(Module, Filter).

do_log_sinks([], _Request, _Level, _Levels) ->
    ok;
do_log_sinks([Sink|Rest], Request, Level, Levels) ->
    do_log_sink(Sink, Request, Level, Levels),
    do_log_sinks(Rest, Request, Level, Levels).

%% @private
do_log_sink({Module, Function} = _Sink, Request, Level, Levels) ->
    case lists:member(Level, Levels) of
        true ->
            try
                Module:Function(Request)
            catch
                _:_ ->
                    io:format(
                        "An error occurred attempting to log to sink ~p:~p/1.  request=~p~n",
                        [Module, Function, Request]
                    )
            end;
        _ ->
            ok
    end.

%% @private
fill_defaults(Config, Defaults) ->
    fill_defaults(Config, Defaults, []).

%% @private
fill_defaults(_Config, [], Accum) ->
    Accum;
fill_defaults(Config, [{K,_V}=H|T], Accum) ->
    case proplists:get_value(K, Config) of
        undefined ->
            fill_defaults(Config, T, [H|Accum]);
        Value ->
            fill_defaults(Config, T, [{K,Value}|Accum])
    end.
