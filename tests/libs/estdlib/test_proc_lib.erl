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
-export([init_ok/1, init_crash/1, init_initial_call_ancestors/1]).

test() ->
    ok = test_start_sync(),
    ok = test_start_monitor_badarg(),
    ok = test_start_link_sync(),
    ok = test_start_link_opt_sync(),
    ok = test_start_timeout(),
    ok = test_start_crash(),
    ok = test_initial_call_and_ancestors(),
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
    exit(Pid, normal),
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
    exit(Pid, normal),
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
    exit(Pid, normal),
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
