%
% This file is part of AtomVM.
%
% Copyright 2019-2023 Fred Dushin <fred@dushin.net>
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
%% @doc An implementation of the Erlang/OTP gen_statem interface.
%%
%% This module implements a strict subset of the Erlang/OTP gen_statem
%% interface, supporting operations for local creation and management of
%% gen_statem instances.
%%
%% This module is designed to be API-compatible with gen_statem, with exceptions noted
%% below.
%%
%% Caveats:
%% <ul>
%%     <li>No support for start_link</li>
%%     <li>Support only for locally named gen_statem instances</li>
%%     <li>Support only for state function event handlers</li>
%%     <li>No support for keep_state or repeat_state return values from Module:StateName/3 callbacks</li>
%%     <li>No support for postpone or hibernate state transition actions</li>
%%     <li>No support for state enter calls</li>
%%     <li>No support for multi_call</li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------
-module(gen_statem).

-export([
    start/3, start/4, start_link/3, start_link/4, stop/1, stop/3, call/2, call/3, cast/2, reply/2
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    mod :: module(),
    current_state :: atom(),
    data :: term(),
    timer_map = #{} :: map()
}).

-type options() :: list({atom(), term()}).
-type server_ref() :: atom() | pid().
-type from() :: {pid(), reference()}.

-type action() ::
    {reply, From :: pid(), Reply :: any()}
    | {state_timeout, Timeout :: timeout(), Msg :: any()}.

-type init_result(StateType, DataType) ::
    {ok, State :: StateType, Data :: DataType}
    | {ok, State :: StateType, Data :: DataType, Actions :: [action()]}
    | {stop, Reason :: any()}.

-callback init(Args :: any()) -> init_result(any(), any()).
-callback terminate(Reason :: normal | any(), State :: any(), Data :: any()) ->
    any().

%%-----------------------------------------------------------------------------
%% @param   ServerName the name with which to register the gen_statem
%% @param   Module the module in which the gen_statem callbacks are defined
%% @param   Args the arguments to pass to the module's init callback
%% @param   Options the options used to create the gen_statem
%% @returns the gen_statem pid, if successful; {error, Reason}, otherwise.
%% @doc     Start a named gen_statem.
%%
%%          This function will start a gen_statem instance and register the
%%          newly created process with the process registry.  Subsequent calls
%%          may use the gen_statem name, in lieu of the process id.
%%
%%          <em><b>Note.</b>  The Options argument is currently ignored.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec start(
    ServerName :: {local, Name :: atom()},
    Module :: module(),
    Args :: term(),
    Options :: options()
) -> {ok, pid()} | {error, Reason :: term()}.
start({local, Name} = ServerName, Module, Args, Options) when is_atom(Name) ->
    gen_server:start(ServerName, ?MODULE, {Module, Args}, Options).

%%-----------------------------------------------------------------------------
%% @param   Module the module in which the gen_statem callbacks are defined
%% @param   Args the arguments to pass to the module's init callback
%% @param   Options the options used to create the gen_statem
%% @returns the gen_statem pid, if successful; {error, Reason}, otherwise.
%% @doc     Start an un-named gen_statem.
%%
%%          This function will start a gen_statem instance.
%%
%%          <em><b>Note.</b>  The Options argument is currently ignored.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec start(Module :: module(), Args :: term(), Options :: options()) ->
    {ok, pid()} | {error, Reason :: term()}.
start(Module, Args, Options) ->
    gen_server:start(?MODULE, {Module, Args}, Options).

%%-----------------------------------------------------------------------------
%% @param   ServerName the name with which to register the gen_statem
%% @param   Module the module in which the gen_statem callbacks are defined
%% @param   Args the arguments to pass to the module's init callback
%% @param   Options the options used to create the gen_statem
%% @returns the gen_statem pid, if successful; {error, Reason}, otherwise.
%% @doc     Start a named gen_statem.
%%
%%          This function will start a gen_statem instance and register the
%%          newly created process with the process registry.  Subsequent calls
%%          may use the gen_statem name, in lieu of the process id.
%%
%%          This version of the start function will link the started gen_statem
%%          process to the calling process.
%%
%%          <em><b>Note.</b>  The Options argument is currently ignored.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(
    ServerName :: {local, Name :: atom()},
    Module :: module(),
    Args :: term(),
    Options :: options()
) -> {ok, pid()} | {error, Reason :: term()}.
start_link({local, Name} = ServerName, Module, Args, Options) when is_atom(Name) ->
    gen_server:start_link(ServerName, ?MODULE, {Module, Args}, Options).

%%-----------------------------------------------------------------------------
%% @param   Module the module in which the gen_statem callbacks are defined
%% @param   Args the arguments to pass to the module's init callback
%% @param   Options the options used to create the gen_statem
%% @returns the gen_statem pid, if successful; {error, Reason}, otherwise.
%% @doc     Start an un-named gen_statem.
%%
%%          This function will start a gen_statem instance.
%%
%%          This version of the start function will link the started gen_statem
%%          process to the calling process.
%%
%%          <em><b>Note.</b>  The Options argument is currently ignored.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(Module :: module(), Args :: term(), Options :: options()) ->
    {ok, pid()} | {error, Reason :: term()}.
start_link(Module, Args, Options) ->
    gen_server:start_link(?MODULE, {Module, Args}, Options).

%%-----------------------------------------------------------------------------
%% @equiv   stop(ServerRef, normal, infinity)
%% @doc     Stop a previously started gen_statem.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(ServerRef :: server_ref()) -> ok | {error, Reason :: term()}.
stop(ServerRef) ->
    gen_server:stop(ServerRef).

%%-----------------------------------------------------------------------------
%% @param   ServerRef a reference to the gen_statem acquired via start
%% @param   Reason the reason to supply for stopping
%% @param   Timeout maximum time to wait for shutdown
%% @returns ok, if the gen_statem stopped; {error, Reason}, otherwise.
%% @doc     Stop a previously started gen_statem instance.
%%
%%          This function will stop a gen_statem instance, providing the supplied
%%          Reason to the .  If the gen_statem is
%%          a named gen_statem, then the gen_statem name may be used to stop the gen_statem.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(ServerRef :: server_ref(), Reason :: term(), Timeout :: non_neg_integer() | infinity) ->
    ok | {error, Reason :: term()}.
stop(ServerRef, Reason, Timeout) ->
    gen_server:stop(ServerRef, Reason, Timeout).

%%-----------------------------------------------------------------------------
%% @equiv   call(ServerRef, Request, infinity)
%% @doc     Send a request to a gen_statem instance, and wait for a reply.
%% @end
%%-----------------------------------------------------------------------------
-spec call(ServerRef :: server_ref(), Request :: term()) ->
    Reply :: term() | {error, Reason :: term()}.
call(ServerRef, Request) ->
    call(ServerRef, Request, 5000).

%%-----------------------------------------------------------------------------
%% @param   ServerRef a reference to the gen_statem acquired via start
%% @param   Request the request to send to the gen_statem
%% @param   Timeout the amount of time in milliseconds to wait for a reply
%% @returns the reply sent back from the gen_statem; {error, Reason}, otherwise.
%% @doc     Send a request to a gen_statem instance, and wait for a reply..
%%
%%          This function will send the specified request to the specified
%%          gen_statem instance, and wait at least Timeout milliseconds for a
%%          reply from the gen_statem.
%% @end
%%-----------------------------------------------------------------------------
-spec call(ServerRef :: server_ref(), Request :: term(), Timeout :: timeout()) ->
    Reply :: term() | {error, Reason :: term()}.
call(ServerRef, Request, Timeout) ->
    try gen:call(ServerRef, '$gen_call', Request, Timeout) of
        {ok, Reply} -> Reply
    catch
        exit:Reason ->
            exit({Reason, {?MODULE, ?FUNCTION_NAME, [ServerRef, Request]}})
    end.

%%-----------------------------------------------------------------------------
%% @param   ServerRef a reference to the gen_statem acquired via start
%% @param   Request the request to send to the gen_statem
%% @returns ok | {error, Reason}
%% @doc     Send a request to a gen_statem instance.
%%
%%          This function will send the specified request to the specified
%%          gen_statem instance, but will not wait for a reply.
%% @end
%%-----------------------------------------------------------------------------
-spec cast(ServerRef :: server_ref(), Request :: term()) -> ok | {error, Reason :: term()}.
cast(ServerRef, Request) ->
    gen:cast(ServerRef, Request).

%%-----------------------------------------------------------------------------
%% @param   Client the client to whom to send the reply
%% @param   Reply the reply to send to the client
%% @returns `ok'
%% @doc     Send a reply to a calling client.
%%
%%          This function will send the specified reply back to the specified
%%          gen_statem client (e.g, via call/3).
%% @end
%%-----------------------------------------------------------------------------
-spec reply(From :: from(), Reply :: term()) -> ok.
reply(From, Reply) ->
    gen:reply(From, Reply).

%%
%% gen_statem callbacks
%%

%% @hidden
init({Module, Args}) ->
    case Module:init(Args) of
        {ok, NextState, Data} ->
            {ok, #state{mod = Module, current_state = NextState, data = Data}};
        {ok, NextState, Data, Actions} ->
            handle_actions(Actions, [{next_state, NextState}]),
            {ok, #state{mod = Module, current_state = NextState, data = Data}};
        {stop, Reason} ->
            {stop, Reason};
        _ ->
            {stop, {error, undefined}}
    end.

%% @hidden
handle_call(Request, From, State) ->
    do_handle_state({call, From}, Request, State).

%% @hidden
handle_cast(Request, State) ->
    do_handle_state(cast, Request, State).

%% @hidden
handle_info(
    {timeout, _TimerRef, {state_timeout, CurrentState, Msg}},
    #state{current_state = CurrentState} = State
) ->
    do_handle_state(state_timeout, Msg, State);
handle_info(Request, State) ->
    do_handle_state(info, Request, State).

%% @hidden
terminate(Reason, #state{mod = Module, current_state = CurrentState, data = Data} = _State) ->
    Module:terminate(Reason, CurrentState, Data),
    ok.

%%
%% Internal operations
%%

%% @private
do_handle_state(EventType, Request, State) ->
    #state{
        mod = Module,
        current_state = CurrentState,
        data = Data,
        timer_map = TimerMap
    } = State,
    case Module:CurrentState(EventType, Request, Data) of
        {next_state, CurrentState, NewData} ->
            {noreply, State#state{data = NewData}};
        {next_state, CurrentState, NewData, Actions} ->
            TimerRefs = handle_actions(Actions, [{next_state, CurrentState}]),
            {noreply, State#state{data = NewData, timer_map = maps:merge(TimerMap, TimerRefs)}};
        %%
        %% if we are transitioning to a new state, cancel any timers associated with this state
        %%
        {next_state, NextState, NewData} ->
            NewTimerMap = maybe_cancel_timer(CurrentState, TimerMap),
            {noreply, State#state{
                current_state = NextState, data = NewData, timer_map = NewTimerMap
            }};
        {next_state, NextState, NewData, Actions} ->
            NewTimerMap = maybe_cancel_timer(CurrentState, TimerMap),
            TimerRefs = handle_actions(Actions, [{next_state, NextState}]),
            {noreply, State#state{
                current_state = NextState,
                data = NewData,
                timer_map = maps:merge(NewTimerMap, TimerRefs)
            }};
        {stop, Reason} ->
            NewTimerMap = maybe_cancel_timer(CurrentState, TimerMap),
            {stop, Reason, State#state{timer_map = NewTimerMap}};
        {stop, Reason, NewData} ->
            NewTimerMap = maybe_cancel_timer(CurrentState, TimerMap),
            {stop, Reason, State#state{data = NewData, timer_map = NewTimerMap}};
        {stop_and_reply, Reason, Replies} ->
            NewTimerMap = maybe_cancel_timer(CurrentState, TimerMap),
            handle_actions(Replies, []),
            {stop, Reason, State#state{timer_map = NewTimerMap}};
        {stop_and_reply, Reason, Replies, NewData} ->
            NewTimerMap = maybe_cancel_timer(CurrentState, TimerMap),
            handle_actions(Replies, []),
            {stop, Reason, State#state{data = NewData, timer_map = NewTimerMap}};
        Reply ->
            {error, {unexpected_reply, Reply}}
    end.

%% @private
handle_actions(Actions, Context) ->
    handle_actions(Actions, Context, #{}).

%% @private
handle_actions([], _Context, Accum) ->
    Accum;
handle_actions([{reply, From, Reply} | T], Context, Accum) ->
    reply(From, Reply),
    handle_actions(T, Context, Accum);
handle_actions([{state_timeout, Timeout, Msg} | T], Context, Accum) ->
    NextState = proplists:get_value(next_state, Context),
    TimerRef = erlang:start_timer(Timeout, self(), {state_timeout, NextState, Msg}),
    handle_actions(T, Context, Accum#{NextState => TimerRef});
handle_actions([_ | T], Context, Accum) ->
    handle_actions(T, Context, Accum).

maybe_cancel_timer(CurrentState, TimerMap) ->
    case maps:find(CurrentState, TimerMap) of
        {ok, TimerRef} ->
            erlang:cancel_timer(TimerRef),
            maps:remove(CurrentState, TimerMap);
        _ ->
            TimerMap
    end.
