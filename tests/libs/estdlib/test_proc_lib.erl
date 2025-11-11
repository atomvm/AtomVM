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

-module(test_proc_lib).

-export([test/0]).
-export([init_ok/1, init_crash/1, init_initial_call_ancestors/1, spawn_func/1]).

test() ->
    ok = test_start_sync(),
    ok = test_start_monitor_badarg(),
    ok = test_start_link_sync(),
    ok = test_start_link_opt_sync(),
    case get_otp_version() of
        Version when Version >= 23 ->
            ok = test_start_monitor_sync();
        _ ->
            ok
    end,
    ok = test_start_timeout(),
    ok = test_start_crash(),
    ok = test_initial_call_and_ancestors(),
    ok = test_spawn(),
    ok.

test_start_sync() ->
    Parent = self(),
    Ret = proc_lib:start(?MODULE, init_ok, [Parent]),
    ok = Ret,
    {ok, Pid} =
        receive
            {Process, inited} -> {ok, Process}
        after 0 -> fail
        end,
    true = is_process_alive(Pid),
    {links, []} = process_info(Pid, links),
    exit(Pid, kill),
    ok.

test_start_monitor_badarg() ->
    Parent = self(),
    ok =
        try
            proc_lib:start(?MODULE, init_ok, [Parent], infinity, [monitor]),
            unexpected
        catch
            error:badarg ->
                ok
        end,
    ok.

test_start_link_sync() ->
    Parent = self(),
    Ret = proc_lib:start_link(?MODULE, init_ok, [Parent]),
    ok = Ret,
    {ok, Pid} =
        receive
            {Process, inited} -> {ok, Process}
        after 0 -> fail
        end,
    true = is_process_alive(Pid),
    {links, [Parent]} = process_info(Pid, links),
    unlink(Pid),
    exit(Pid, kill),
    ok.

test_start_link_opt_sync() ->
    Parent = self(),
    Ret = proc_lib:start(?MODULE, init_ok, [Parent], infinity, [link]),
    ok = Ret,
    {ok, Pid} =
        receive
            {Process, inited} -> {ok, Process}
        after 0 -> fail
        end,
    true = is_process_alive(Pid),
    {links, [Parent]} = process_info(Pid, links),
    unlink(Pid),
    exit(Pid, kill),
    ok.

test_start_monitor_sync() ->
    Parent = self(),
    {Ret, Monitor} = proc_lib:start_monitor(?MODULE, init_ok, [Parent]),
    ok = Ret,
    true = is_reference(Monitor),
    {ok, Pid} =
        receive
            {Process, inited} -> {ok, Process}
        after 0 -> fail
        end,
    true = is_process_alive(Pid),
    {links, []} = process_info(Pid, links),
    exit(Pid, kill),
    killed =
        receive
            {'DOWN', Monitor, process, Pid, Reason} -> Reason
        after 500 -> timeout
        end,
    ok.

test_start_timeout() ->
    Parent = self(),
    Ret = proc_lib:start(?MODULE, init_ok, [Parent], 100),
    {error, timeout} = Ret,
    ok.

test_start_crash() ->
    case erlang:system_info(machine) of
        "ATOM" ->
            {ok, _Pid} = logger_manager:start_link(#{});
        "BEAM" ->
            ok
    end,
    RetError = proc_lib:start(?MODULE, init_crash, [{error, badarg, []}]),
    {error, {badarg, ErrorSt}} = RetError,
    % BEAM returns [] as the stacktrace, AtomVM (currently) returns a
    % full stacktrace if stacktraces are enabled or undefined if
    % stacktraces are disabled
    case erlang:system_info(machine) of
        "ATOM" when ErrorSt =:= undefined ->
            ok;
        "ATOM" ->
            true = is_list(ErrorSt);
        "BEAM" ->
            [] = ErrorSt
    end,
    RetExit = proc_lib:start(?MODULE, init_crash, [{exit, tested, []}]),
    {error, tested} = RetExit,
    RetThrow = proc_lib:start(?MODULE, init_crash, [{throw, tested, []}]),
    {error, {{nocatch, tested}, ThrowSt}} = RetThrow,
    case erlang:system_info(machine) of
        "ATOM" when ThrowSt =:= undefined ->
            ok;
        "ATOM" ->
            true = is_list(ThrowSt);
        "BEAM" ->
            [] = ThrowSt
    end,

    case erlang:system_info(machine) of
        "ATOM" ->
            logger_manager:stop();
        "BEAM" ->
            ok
    end,
    ok.

test_initial_call_and_ancestors() ->
    Parent = self(),
    ImaginaryAncestor = spawn(fun() ->
        receive
            quit -> ok
        end
    end),
    put('$ancestors', [ImaginaryAncestor]),
    Ret = proc_lib:start(?MODULE, init_initial_call_ancestors, [Parent]),
    {ok, {?MODULE, init_initial_call_ancestors, 1}, [Parent, ImaginaryAncestor]} = Ret,
    exit(ImaginaryAncestor, normal),
    erase('$ancestors'),
    ok.

spawn_func(Parent) ->
    {links, Links} = process_info(self(), links),
    Parent ! {self(), ancestors, get('$ancestors')},
    Parent ! {self(), initial_call, get('$initial_call')},
    Parent ! {self(), links, Links}.

test_spawn() ->
    Parent = self(),
    SpawnPid1 = proc_lib:spawn(fun() -> spawn_func(Parent) end),
    [Parent] =
        receive
            {SpawnPid1, ancestors, Ancestors1} -> Ancestors1
        after 1000 -> timeout
        end,
    {FunInfoModule, _FunInfoFunction1, FunInfoArity} =
        receive
            {SpawnPid1, initial_call, InitialCall1} -> InitialCall1
        after 1000 -> timeout
        end,
    [] =
        receive
            {SpawnPid1, links, Links1} -> Links1
        after 1000 -> timeout
        end,
    SpawnPid2 = proc_lib:spawn(?MODULE, spawn_func, [Parent]),
    [Parent] =
        receive
            {SpawnPid2, ancestors, Ancestors2} -> Ancestors2
        after 1000 -> timeout
        end,
    {test_proc_lib, spawn_func, 1} =
        receive
            {SpawnPid2, initial_call, InitialCall2} -> InitialCall2
        after 1000 -> timeout
        end,
    [] =
        receive
            {SpawnPid2, links, Links2} -> Links2
        after 1000 -> timeout
        end,
    SpawnPid3 = proc_lib:spawn_link(fun() -> spawn_func(Parent) end),
    [Parent] =
        receive
            {SpawnPid3, ancestors, Ancestors3} -> Ancestors3
        after 1000 -> timeout
        end,
    {FunInfoModule, _FunInfoFunction2, FunInfoArity} =
        receive
            {SpawnPid3, initial_call, InitialCall3} -> InitialCall3
        after 1000 -> timeout
        end,
    [Parent] =
        receive
            {SpawnPid3, links, Links3} -> Links3
        after 1000 -> timeout
        end,
    SpawnPid4 = proc_lib:spawn_link(?MODULE, spawn_func, [Parent]),
    [Parent] =
        receive
            {SpawnPid4, ancestors, Ancestors4} -> Ancestors4
        after 1000 -> timeout
        end,
    {test_proc_lib, spawn_func, 1} =
        receive
            {SpawnPid4, initial_call, InitialCall4} -> InitialCall4
        after 1000 -> timeout
        end,
    [Parent] =
        receive
            {SpawnPid4, links, Links4} -> Links4
        after 1000 -> timeout
        end,
    ok.

init_ok(Parent) ->
    ok =
        receive
            timer -> fail
        after 500 -> ok
        end,
    Parent ! {self(), inited},
    proc_lib:init_ack(Parent, ok),
    receive
        quit -> ok
    end.

init_crash({Class, Reason, Stacktrace}) ->
    erlang:raise(Class, Reason, Stacktrace).

init_initial_call_ancestors(Parent) ->
    proc_lib:init_ack(Parent, {ok, get('$initial_call'), get('$ancestors')}).

get_otp_version() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            list_to_integer(erlang:system_info(otp_release));
        _ ->
            atomvm
    end.
