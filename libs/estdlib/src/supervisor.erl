%
% This file is part of AtomVM.
%
% Copyright 2021 Davide Bettio <davide@uninstall.it>
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

-module(supervisor).

-behavior(gen_server).

-export([
    start_link/2,
    start_link/3,
    start_child/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-export_type([
    child_spec/0,
    strategy/0,
    sup_flags/0
]).

-type restart() :: permanent | transient | temporary.
-type shutdown() :: brutal_kill | timeout().
-type child_type() :: worker | supervisor.

-type strategy() :: one_for_all | one_for_one.
-type sup_flags() ::
    #{
        strategy => strategy(),
        intensity => non_neg_integer(),
        period => pos_integer()
    }
    | {RestartStrategy :: strategy(), Intensity :: non_neg_integer(), Period :: pos_integer()}.

-type child_spec() ::
    #{
        id := any(),
        start := {module(), atom(), [any()]},
        restart => restart(),
        shutdown => shutdown(),
        type => child_type(),
        modules => [module()] | dynamic
    }
    | {
        Id :: any(),
        StartFunc :: {module(), atom(), [any()]},
        Restart :: restart(),
        Shutdown :: shutdown(),
        Type :: child_type(),
        Modules :: [module()] | dynamic
    }.

-record(child, {
    pid = undefined,
    id :: any(),
    start :: {module(), atom(), [any()] | undefined},
    restart :: restart(),
    shutdown :: shutdown(),
    type :: child_type
}).
-record(state, {restart_strategy :: strategy(), children = [] :: [#child{}]}).

start_link(Module, Args) ->
    gen_server:start_link(?MODULE, {Module, Args}, []).
start_link(SupName, Module, Args) ->
    gen_server:start_link(SupName, ?MODULE, {Module, Args}, []).

start_child(Supervisor, ChildSpec) ->
    gen_server:call(Supervisor, {start_child, ChildSpec}).

init({Mod, Args}) ->
    erlang:process_flag(trap_exit, true),
    case Mod:init(Args) of
        {ok, {{Strategy, _Intensity, _Period}, StartSpec}} ->
            State = init_state(StartSpec, #state{restart_strategy = Strategy}),
            NewChildren = start_children(State#state.children, []),
            {ok, State#state{children = NewChildren}};
        {ok, {#{strategy := Strategy}, StartSpec}} ->
            State = init_state(StartSpec, #state{restart_strategy = Strategy}),
            NewChildren = start_children(State#state.children, []),
            {ok, State#state{children = NewChildren}};
        Error ->
            {stop, {bad_return, {mod, init, Error}}}
    end.

-spec child_spec_to_record(child_spec()) -> #child{}.
child_spec_to_record({ChildId, MFA, Restart, Shutdown, Type, _Modules}) ->
    #child{
        id = ChildId,
        start = MFA,
        restart = Restart,
        shutdown = Shutdown,
        type = Type
    };
child_spec_to_record(#{id := ChildId, start := MFA} = ChildMap) ->
    Restart = maps:get(restart, ChildMap, permanent),
    Type = maps:get(type, ChildMap, worker),
    Shutdown = maps:get(
        shutdown,
        ChildMap,
        case Type of
            worker -> 5000;
            supervisor -> infinity
        end
    ),
    #child{
        id = ChildId,
        start = MFA,
        restart = Restart,
        shutdown = Shutdown,
        type = Type
    }.

init_state([ChildSpec | T], State) ->
    Child = child_spec_to_record(ChildSpec),
    NewChildren = [Child | State#state.children],
    init_state(T, #state{children = NewChildren});
init_state([], State) ->
    State#state{children = lists:reverse(State#state.children)}.

start_children([Child | T], StartedC) ->
    case try_start(Child) of
        {ok, Pid, _Result} ->
            start_children(T, [Child#child{pid = Pid} | StartedC])
    end;
start_children([], StartedC) ->
    StartedC.

restart_child(Pid, Reason, State) ->
    case lists:keyfind(Pid, #child.pid, State#state.children) of
        false ->
            {ok, State};
        #child{} = Child ->
            case should_restart(Reason, Child#child.restart) of
                true ->
                    case try_start(Child) of
                        {ok, NewPid, _Result} ->
                            NewChild = Child#child{pid = NewPid},
                            Children = lists:keyreplace(
                                Pid, #child.pid, State#state.children, NewChild
                            ),
                            {ok, State#state{children = Children}}
                    end;
                false ->
                    Children = lists:keydelete(Pid, #child.pid, State#state.children),
                    {ok, State#state{children = Children}}
            end
    end.

should_restart(_Reason, permanent) ->
    true;
should_restart(_Reason, temporary) ->
    false;
should_restart(Reason, transient) ->
    case Reason of
        normal -> false;
        _any -> true
    end.

handle_call({start_child, ChildSpec}, _From, #state{children = Children} = State) ->
    Child = child_spec_to_record(ChildSpec),
    #child{id = ID} = Child,
    case lists:keyfind(ID, #child.id, State#state.children) of
        #child{pid = undefined} ->
            {reply, {error, already_present}, State};
        #child{pid = Pid} ->
            {reply, {error, {already_started, Pid}}, State};
        false ->
            case try_start(Child) of
                {ok, Pid, Result} ->
                    UpdatedChild = Child#child{pid = Pid},
                    {reply, Result, State#state{children = [UpdatedChild | Children]}};
                {error, _Reason} = ErrorT ->
                    {reply, ErrorT, State}
            end
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    case restart_child(Pid, Reason, State) of
        {ok, State1} ->
            {noreply, State1};
        {shutdown, State1} ->
            {stop, shutdown, State1}
    end;
handle_info(_Msg, State) ->
    %TODO: log unexpected message
    {noreply, State}.

try_start(#child{start = {M, F, Args}} = Record) ->
    try
        case apply(M, F, Args) of
            {ok, Pid} when is_pid(Pid) ->
                {ok, Pid, {ok, Pid}};
            {ok, Pid, Info} when is_pid(Pid) ->
                {ok, Pid, {ok, Pid, Info}};
            ignore ->
                {ok, undefined, {ok, undefined}};
            {error, Reason} ->
                {error, {Reason, Record}};
            Other ->
                {error, {Other, Record}}
        end
    catch
        error:Error ->
            {error, {{'EXIT', Error}, Record}}
    end.
