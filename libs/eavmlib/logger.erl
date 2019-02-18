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

-export([start/0, start/1, log/3, get_levels/0, set_levels/1, get_filter/0, set_filter/1, stop/0]).
-export([loop/1]).

-include("estdlib.hrl").

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
    start([{levels, [info, warning, error]}, {filter, []}]).

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
-spec log(Location::location(), Level::level(), Msg::term()) -> ok.
log(Location, Level, Msg) ->
    {ok, Pid} = maybe_start(whereis(?MODULE)),
    Pid ! {Location, erlang:universaltime(), self(), Level, Msg},
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
    Levels = ?PROPLISTS:get_value(levels, Config, []),
    Filter = ?PROPLISTS:get_value(filter, Config, []),
    receive
        {get_levels, Ref, Pid} ->
            Pid ! {Ref, Levels},
            loop(State);
        {get_filter, Ref, Pid} ->
            Pid ! {Ref, Filter},
            loop(State);
        {set_levels, NewLevels} ->
            loop(State#state{config=[{levels, NewLevels} | ?LISTS:keydelete(levels, 1, Config)]});
        {set_filter, NewFilter} ->
            loop(State#state{config=[{filter, NewFilter} | ?LISTS:keydelete(filter, 1, Config)]});
        {_Location, _Time, _Pid, Level, _Msg} = Request ->
            do_log(Request, Level, Levels, Filter),
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
do_log(_Request, _Level, undefined, _Filter) ->
    ok;
do_log({Location, _Time, _Pid, _Level, _Msg} = Request, Level, Levels, Filter) ->
    case match_filter(Location, Filter) of
        true ->
            do_log(Request, Level, Levels);
        _ ->
            ok
    end.

match_filter(_Location, []) ->
    true;
match_filter({Module, _Function, _Arity, _Line}, Filter) ->
    ?LISTS:member(Module, Filter).

do_log(Request, Level, Levels) ->
    case ?LISTS:member(Level, Levels) of
        true ->
            erlang:display(Request);
        _ ->
            ok
    end.
