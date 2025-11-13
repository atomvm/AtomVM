%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(test_sys).

-export([test/0, system_continue/3, system_terminate/4, system_code_change/4]).
-export([start/0]).

start() ->
    ok = test().

test() ->
    ok = test_get_state_status(),
    ok = test_replace_state(),
    ok = test_suspend_resume(),
    ok = test_change_code(),
    ok = test_trace(),
    ok.

test_get_state_status() ->
    Parent = self(),
    {SysPid, MonitorRef} = spawn_opt(fun() -> system_continue(Parent, [], state) end, [
        link, monitor
    ]),
    state = sys:get_state(SysPid),
    {status, SysPid, {module, ?MODULE}, _Extra} = sys:get_status(SysPid),
    ok = sys:terminate(SysPid, normal),
    normal =
        receive
            {'DOWN', MonitorRef, process, SysPid, Reason} -> Reason
        after 1000 -> timeout
        end,
    ok.

test_replace_state() ->
    Parent = self(),
    {SysPid, MonitorRef} = spawn_opt(fun() -> system_continue(Parent, [], {state, 0}) end, [
        link, monitor
    ]),
    {state, 1} = sys:replace_state(SysPid, fun({state, 0}) -> {state, 1} end),
    {state, 1} = sys:get_state(SysPid),
    ok =
        try
            sys:replace_state(SysPid, fun({state, 0}) -> {state, 1} end),
            unexpected
        catch
            error:{callback_failed, _, {error, function_clause}} -> ok
        end,
    ok = sys:terminate(SysPid, normal),
    normal =
        receive
            {'DOWN', MonitorRef, process, SysPid, Reason} -> Reason
        after 1000 -> timeout
        end,
    ok.

test_suspend_resume() ->
    Parent = self(),
    {SysPid, MonitorRef} = spawn_opt(fun() -> system_continue(Parent, [], {state, 0}) end, [
        link, monitor
    ]),
    SysPid ! {test_sys_ping, self()},
    ok =
        receive
            {SysPid, pong} -> ok
        after 1000 -> timeout
        end,
    ok = sys:suspend(SysPid),
    SysPid ! {test_sys_ping, self()},
    ok =
        receive
            {SysPid, pong} -> unexpected
        after 500 -> ok
        end,
    ok = sys:resume(SysPid),
    ok =
        receive
            {SysPid, pong} -> ok
        after 1000 -> timeout
        end,
    ok = sys:terminate(SysPid, normal),
    normal =
        receive
            {'DOWN', MonitorRef, process, SysPid, Reason} -> Reason
        after 1000 -> timeout
        end,
    ok.

test_change_code() ->
    Parent = self(),
    {SysPid, MonitorRef} = spawn_opt(fun() -> system_continue(Parent, [], state) end, [
        link, monitor
    ]),
    state = sys:get_state(SysPid),
    ok = sys:suspend(SysPid),
    ok = sys:change_code(SysPid, module, "1", extra),
    ok = sys:resume(SysPid),
    {state, module, "1", extra} = sys:get_state(SysPid),
    ok = sys:terminate(SysPid, normal),
    normal =
        receive
            {'DOWN', MonitorRef, process, SysPid, Reason} -> Reason
        after 1000 -> timeout
        end,
    ok.

test_trace() ->
    Parent = self(),
    {SysPid, MonitorRef} = spawn_opt(fun() -> system_continue(Parent, [], state) end, [
        link, monitor
    ]),
    state = sys:get_state(SysPid),
    ok =
        receive
            {debug_event, SysPid, _, _, _} = Msg -> {unexpected, Msg}
        after 100 -> ok
        end,
    ok = sys:trace(SysPid, true),
    state = sys:get_state(SysPid),
    ok =
        receive
            {debug_event, SysPid, standard_io, get_state, state} -> ok
        after 500 -> timeout
        end,
    ok = sys:trace(SysPid, false),
    ok =
        receive
            {debug_event, SysPid, standard_io, {debug, {trace, false}}, state} -> ok
        after 500 -> timeout
        end,
    state = sys:get_state(SysPid),
    ok =
        receive
            {debug_event, SysPid, _, _, _} = Msg3 -> {unexpected, Msg3}
        after 100 -> ok
        end,
    ok = sys:terminate(SysPid, normal),
    normal =
        receive
            {'DOWN', MonitorRef, process, SysPid, Reason} -> Reason
        after 1000 -> timeout
        end,
    ok.

system_continue(Parent, Debug, State) ->
    receive
        {system, From, Msg} ->
            sys:handle_debug(
                Debug,
                fun(Dev, Event, EventState) -> format_event(Parent, Dev, Event, EventState) end,
                State,
                Msg
            ),
            sys:handle_system_msg(Msg, From, Parent, ?MODULE, Debug, State);
        {test_sys_ping, Pid} ->
            Pid ! {self(), pong},
            system_continue(Parent, Debug, State);
        Other ->
            sys:handle_debug(
                Debug,
                fun(Dev, Event, EventState) -> format_event(Parent, Dev, Event, EventState) end,
                State,
                {unexpected, Other}
            ),
            system_continue(Parent, Debug, State)
    end.

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

system_code_change(State, Module, Vsn, Extra) ->
    {ok, {State, Module, Vsn, Extra}}.

format_event(Parent, Dev, Event, State) ->
    Parent ! {debug_event, self(), Dev, Event, State}.
