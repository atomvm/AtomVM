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
    start_link/2, start_link/3
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(child, {pid = undefined, id, mfargs, restart_type, shutdown, child_type, modules = []}).
-record(state, {children = []}).

start_link(Module, Args) ->
    gen_server:start_link(?MODULE, {Module, Args}, []).
start_link(SupName, Module, Args) ->
    gen_server:start_link(SupName, ?MODULE, {Module, Args}, []).

init({Mod, Args}) ->
    erlang:process_flag(trap_exit, true),
    case Mod:init(Args) of
        {ok, {{one_for_one, _Intensity, _Period}, StartSpec}} ->
            State = init_state(StartSpec, #state{}),
            NewChildren = start_children(State#state.children, []),
            {ok, State#state{children = NewChildren}};
        Error ->
            {stop, {bad_return, {mod, init, Error}}}
    end.

init_state([{ChildId, MFA, Restart, brutal_kill, Type, Modules} | T], State) ->
    Child = #child{
        id = ChildId,
        mfargs = MFA,
        restart_type = Restart,
        shutdown = brutal_kill,
        child_type = Type,
        modules = Modules
    },
    NewChildren = [Child | State#state.children],
    init_state(T, #state{children = NewChildren});
init_state([], State) ->
    State#state{children = lists:reverse(State#state.children)}.

start_children([Child | T], StartedC) ->
    #child{mfargs = {M, F, Args}} = Child,
    case apply(M, F, Args) of
        {ok, Pid} when is_pid(Pid) ->
            start_children(T, [Child#child{pid = Pid} | StartedC])
    end;
start_children([], StartedC) ->
    StartedC.

restart_child(Pid, Reason, State) ->
    Child = lists:keyfind(Pid, #child.pid, State#state.children),

    #child{mfargs = {M, F, Args}} = Child,
    case should_restart(Reason, Child#child.restart_type) of
        true ->
            case apply(M, F, Args) of
                {ok, NewPid} when is_pid(Pid) ->
                    NewChild = Child#child{pid = NewPid},
                    Children = lists:keyreplace(Pid, #child.pid, State#state.children, NewChild),
                    {ok, State#state{children = Children}}
            end;
        false ->
            Children = lists:keydelete(Pid, #child.pid, State#state.children),
            {ok, State#state{children = Children}}
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

handle_call(_Msg, _from, State) ->
    {noreply, State}.

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
