%
% This file is part of AtomVM.
%
% Copyright 2018-2022 Fred Dushin <fred@dushin.net>
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
%% @hidden
%%-----------------------------------------------------------------------------
-module(timer_manager).

-export([start/0, start_timer/3, cancel_timer/1, get_timer_refs/0, send_after/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([run_timer/5, send_after_timer/3]).

-record(state, {
    timers = [] :: [{reference(), pid()}]
}).

-define(SERVER_NAME, ?MODULE).

-spec start() -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start() ->
    gen_server:start({local, ?SERVER_NAME}, ?MODULE, [], []).

-spec maybe_start() -> {ok, Pod :: pid()}.
maybe_start() ->
    case start() of
        {ok, _Pid} = R -> R;
        {error, {already_started, Pid}} -> {ok, Pid}
    end.

%%-----------------------------------------------------------------------------
%% @param   Time time in milliseconds after which to send the timeout message.
%% @param   Dest Pid or server name to which to send the timeout message.
%% @param   Msg Message to send to Dest after Time ms.
%% @returns a reference that can be used to cancel the timer, if desired.
%% @doc     Start a timer, and send {timeout, TimerRef, Msg} to Dest after
%%          Time ms, where TimerRef is the reference returned from this function.
%% @end
%%-----------------------------------------------------------------------------
-spec start_timer(Time :: non_neg_integer(), Dest :: pid(), Msg :: term()) ->
    TimerRef :: reference().
start_timer(Time, Dest, Msg) ->
    maybe_start(),
    gen_server:call(?SERVER_NAME, {Time, Dest, Msg}).

%%-----------------------------------------------------------------------------
%% @hidden
%% @param   Time time in milliseconds after which to send the timeout message.
%% @param   Dest Pid or server name to which to send the timeout message.
%% @param   Msg Message to send to Dest after Time ms.
%% @param   Options options
%% @returns a reference that can be used to cancel the timer, if desired.
%% @doc     Start a timer, and send {timeout, TimerRef, Msg} to Dest after
%%          Time ms, where TimerRef is the reference returned from this function.
%%
%%          <em><b>Note.</b>  The Options argument is currently ignored.</em>
%%-----------------------------------------------------------------------------
-spec cancel_timer(TimerRef :: reference()) -> ok.
cancel_timer(TimerRef) ->
    maybe_start(),
    gen_server:call(?SERVER_NAME, {cancel, TimerRef}).

-spec get_timer_refs() -> [reference()].
get_timer_refs() ->
    maybe_start(),
    gen_server:call(?SERVER_NAME, get_timer_refs).

%%-----------------------------------------------------------------------------
%% @param   Time time in milliseconds after which to send the message.
%% @param   Dest Pid or server name to which to send the message.
%% @param   Msg Message to send to Dest after Time ms.
%% @returns a reference that can be used to cancel the timer, if desired.
%% @doc     Send Msg to Dest after Time ms.
%% @end
%%-----------------------------------------------------------------------------
-spec send_after(non_neg_integer(), pid() | atom(), term()) -> reference().
send_after(Time, Dest, Msg) ->
    TimerRef = erlang:make_ref(),
    spawn(?MODULE, send_after_timer, [Time, Dest, Msg]),
    TimerRef.

%%
%% ?GEN_SERVER callbacks
%%

%% @hidden
init([]) ->
    {ok, #state{}}.

%% @hidden
handle_call(get_timer_refs, _From, #state{timers = Timers} = State) ->
    {reply, [TimerRef || {TimerRef, _Pid} <- Timers], State};
handle_call({cancel, TimerRef}, From, #state{timers = Timers} = State) ->
    case lists:keyfind(TimerRef, 1, Timers) of
        false ->
            {reply, false, State};
        {TimerRef, Pid} ->
            _MonitorRef = erlang:monitor(process, Pid),
            Pid ! {cancel, From},
            NewTimers = lists:keyreplace(Pid, 2, Timers, {{canceled, From}, Pid}),
            {noreply, State#state{timers = NewTimers}}
    end;
handle_call({Time, Dest, Msg}, _From, #state{timers = Timers} = State) ->
    {TimerRef, Pid} = do_start_timer(Time, Dest, Msg),
    {reply, TimerRef, State#state{timers = [{TimerRef, Pid} | Timers]}}.

%% @hidden
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_info({fired, Pid}, #state{timers = Timers} = State) ->
    NewTimers =
        case lists:keyfind(Pid, 2, Timers) of
            {{canceled, From}, Pid} ->
                gen_server:reply(From, false),
                lists:keydelete(Pid, 2, Timers);
            {_TimerRef, Pid} ->
                lists:keydelete(Pid, 2, Timers);
            false ->
                Timers
        end,
    {noreply, State#state{timers = NewTimers}};
handle_info({canceled, Pid}, #state{timers = Timers} = State) ->
    NewTimers = lists:keydelete(Pid, 2, Timers),
    {noreply, State#state{timers = NewTimers}};
handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, #state{timers = Timers} = State) ->
    case lists:keyfind(Pid, 2, Timers) of
        false ->
            {noreply, State};
        {{canceled, From}, Pid} ->
            gen_server:reply(From, false),
            NewTimers = lists:keydelete(Pid, 2, Timers),
            {noreply, State#state{timers = NewTimers}}
    end.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%%=============================================================================
%% internal functions

%% @private
do_start_timer(Time, Dest, Msg) ->
    TimerRef = erlang:make_ref(),
    Pid = spawn(?MODULE, run_timer, [self(), Time, TimerRef, Dest, Msg]),
    {TimerRef, Pid}.

%% @private
run_timer(MgrPid, Time, TimerRef, Dest, Msg) ->
    Start = erlang:system_time(millisecond),
    receive
        {cancel, From} ->
            MgrPid ! {canceled, self()},
            gen_server:reply(From, Time - (erlang:system_time(millisecond) - Start))
    after Time ->
        MgrPid ! {fired, self()},
        Dest ! {timeout, TimerRef, Msg}
    end.

%% @private
send_after_timer(Time, Dest, Msg) ->
    TimerRef = start_timer(Time, self(), Msg),
    receive
        {timeout, TimerRef, Msg} ->
            Dest ! Msg
    end.
