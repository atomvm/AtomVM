%
% This file is part of AtomVM.
%
% Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

-module(test_monitor).

-export([start/0]).

start() ->
    ok = test_monitor_normal(),
    ok = test_monitor_demonitor(),
    ok = test_monitor_noproc(),
    ok = test_monitor_demonitor_flush(),
    ok = test_monitor_demonitor_info(),
    ok = test_monitor_demonitor_flush_info_true(),
    ok = test_monitor_demonitor_flush_info_false(),
    ok = test_monitor_self(),
    ok = test_monitor_demonitor_from_other(),
    ok = test_monitor_registered(),
    ok = test_monitor_registered_noproc(),

    AliasesAvailable =
        case erlang:system_info(machine) of
            "ATOM" -> true;
            "BEAM" -> erlang:system_info(otp_release) >= "24"
        end,
    if
        AliasesAvailable ->
            ok = test_alias(),
            ok = test_multiple_aliases(),
            ok = test_multiple_unaliases(),
            ok = test_unalias_from_wrong_process(),
            ok = test_monitor_alias_dead_process(),
            ok = test_monitor_multiple_aliases_monitors(fun spawn_monitor/2),
            ok = test_monitor_multiple_aliases_monitors(fun spawn_and_monitor/2),
            ok = test_monitor_alias_demonitor(fun spawn_monitor/2),
            ok = test_monitor_alias_demonitor(fun spawn_and_monitor/2),
            ok = test_monitor_alias_explicit_unalias(fun spawn_monitor/2),
            ok = test_monitor_alias_explicit_unalias(fun spawn_and_monitor/2),
            ok = test_monitor_alias_reply_demonitor(fun spawn_monitor/2),
            ok = test_monitor_alias_reply_demonitor(fun spawn_and_monitor/2),
            ok = test_monitor_down_alias(fun spawn_monitor/2),
            ok = test_monitor_down_alias(fun spawn_and_monitor/2),
            ok;
        true ->
            ok
    end,
    0.

test_monitor_normal() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    Ref = monitor(process, Pid),
    Pid ! {self(), quit},
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref, process, Pid, normal} -> ok;
            Other2 -> {unexpected, Other2}
        after 5000 -> timeout
        end,
    ok.

test_monitor_demonitor() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    Ref = monitor(process, Pid),
    true = demonitor(Ref),
    Pid ! {self(), quit},
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            Other2 -> {unexpected, Other2}
        after 200 -> ok
        end,
    ok.

test_monitor_noproc() ->
    {Pid, Monitor} = spawn_opt(fun() -> ok end, [monitor]),
    ok =
        receive
            {'DOWN', Monitor, process, Pid, normal} -> ok
        after 500 -> timeout
        end,
    Ref = monitor(process, Pid),
    ok =
        receive
            {'DOWN', Ref, process, Pid, noproc} -> ok;
            Other -> {unexpected, Other}
        after 5000 -> timeout
        end,
    ok.

test_monitor_registered() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    true = register(name, Pid),
    Ref = monitor(process, name),
    Pid ! {self(), quit},
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref, process, {name, nonode@nohost}, normal} -> ok;
            Other2 -> {unexpected, Other2}
        after 5000 -> timeout
        end,
    ok.

test_monitor_registered_noproc() ->
    Ref = monitor(process, foobar),
    ok =
        receive
            {'DOWN', Ref, process, {foobar, nonode@nohost}, noproc} -> ok;
            Other -> {unexpected, Other}
        after 5000 -> timeout
        end,
    ok.

test_monitor_demonitor_flush() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    Ref = monitor(process, Pid),
    Pid ! {self(), quit},
    receive
    after 100 -> ok
    end,
    true = demonitor(Ref, [flush]),
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            Other2 -> {unexpected, Other2}
        after 200 -> ok
        end,
    ok.

test_monitor_demonitor_info() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    Ref = monitor(process, Pid),
    true = demonitor(Ref, [info]),
    Pid ! {self(), quit},
    false = demonitor(Ref, [info]),
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            Other2 -> {unexpected, Other2}
        after 200 -> ok
        end,
    ok.

test_monitor_demonitor_flush_info_true() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    Ref = monitor(process, Pid),
    Pid ! {self(), quit},
    receive
    after 100 -> ok
    end,
    false = demonitor(Ref, [flush, info]),
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            Other2 -> {unexpected, Other2}
        after 200 -> ok
        end,
    ok.

test_monitor_demonitor_flush_info_false() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    Ref = monitor(process, Pid),
    true = demonitor(Ref, [flush, info]),
    Pid ! {self(), quit},
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            Other2 -> {unexpected, Other2}
        after 200 -> ok
        end,
    ok.

test_monitor_self() ->
    Ref = monitor(process, self()),
    false = demonitor(Ref, [info]),
    ok.

test_monitor_demonitor_from_other() ->
    Pid = spawn_opt(fun() -> normal_loop() end, []),
    Ref = monitor(process, Pid),
    {OtherPid, OtherRef} = spawn_opt(
        fun() ->
            false = demonitor(Ref, [info])
        end,
        [monitor]
    ),
    normal =
        receive
            {'DOWN', OtherRef, process, OtherPid, Reason} -> Reason
        after 500 -> timeout
        end,
    Pid ! {self(), quit},
    ok =
        receive
            {Pid, finished} -> ok;
            Other1 -> {unexpected, Other1}
        after 5000 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref, process, Pid, normal} -> ok
        after 500 -> timeout
        end,
    ok.

test_alias() ->
    P = spawn_opt(fun echo_loop/0, []),
    Alias = erlang:alias(),
    do_test_alias(P, Alias),
    ok.

test_multiple_aliases() ->
    P = spawn_opt(fun echo_loop/0, []),
    A1 = erlang:alias(),
    A2 = erlang:alias(),
    A3 = erlang:alias(),
    do_test_alias(P, A1),
    do_test_alias(P, A3),
    do_test_alias(P, A2),
    ok.

test_multiple_unaliases() ->
    A = erlang:alias(),
    true = erlang:unalias(A),
    false = erlang:unalias(A),
    false = erlang:unalias(A),
    ok.

test_unalias_from_wrong_process() ->
    A = erlang:alias(),
    TestProcess = self(),
    spawn_opt(fun() -> TestProcess ! erlang:unalias(A) end, [link]),
    false = recv_one(),
    P = spawn_opt(fun echo_loop/0, []),
    do_test_alias(P, A),
    ok.

do_test_alias(P, Alias) ->
    do_test_alias(P, Alias, fun erlang:unalias/1).

do_test_alias(P, Alias, UnaliasFun) ->
    Ref = make_ref(),
    P ! {{m1, Ref}, Alias},
    {m1, Ref} = recv_one(),
    UnaliasFun(Alias),
    P ! {{m2, Ref}, Alias},
    P ! {{m3, Ref}, self()},
    {m3, Ref} = recv_one(),
    ok.

test_monitor_alias_demonitor(SpawnFun) ->
    {P, Mon} = SpawnFun(fun echo_loop/0, [{alias, demonitor}]),
    do_test_alias(P, Mon, fun demonitor/1),
    ok.

test_monitor_alias_explicit_unalias(SpawnFun) ->
    {P, Mon} = SpawnFun(fun echo_loop/0, [{alias, explicit_unalias}]),
    P ! {m1, Mon},
    m1 = recv_one(),
    demonitor(Mon),
    do_test_alias(P, Mon),
    ok.

test_monitor_alias_reply_demonitor(SpawnFun) ->
    {P, Mon} = SpawnFun(fun echo_loop/0, [{alias, reply_demonitor}]),
    do_test_alias(P, Mon, fun(_Mon) -> ok end),
    ok.

test_monitor_down_alias(SpawnFun) ->
    {P, Mon} = SpawnFun(fun echo_loop/0, [{alias, demonitor}]),
    erlang:unalias(Mon),
    P ! {m1, Mon},
    P ! {m2, self()},
    m2 = recv_one(),
    P ! quit,
    {'DOWN', Mon, process, P, normal} = recv_one(),
    ok.

test_monitor_multiple_aliases_monitors(SpawnFun) ->
    {P, Mon1} = SpawnFun(fun echo_loop/0, [{alias, demonitor}]),
    Mon2 = erlang:monitor(process, P, [{alias, reply_demonitor}]),
    Mon3 = erlang:monitor(process, P, [{alias, explicit_unalias}]),
    Mon4 = erlang:monitor(process, P),
    A1 = erlang:alias(),
    A2 = erlang:alias(),
    do_test_alias(P, A2),
    do_test_alias(P, Mon3),
    do_test_alias(P, A1),
    do_test_alias(P, Mon1, fun demonitor/1),
    P ! quit,
    {'DOWN', Mon2, process, P, normal} = recv_one(),
    {'DOWN', Mon3, process, P, normal} = recv_one(),
    {'DOWN', Mon4, process, P, normal} = recv_one(),
    ok.

test_monitor_alias_dead_process() ->
    {P, Mon0} = spawn_opt(fun() -> ok end, [monitor]),
    {'DOWN', Mon0, process, P, normal} = recv_one(),
    Mon1 = erlang:monitor(process, P, [{alias, demonitor}]),
    {'DOWN', Mon1, process, P, noproc} = recv_one(),
    Mon2 = erlang:monitor(process, P, [{alias, reply_demonitor}]),
    {'DOWN', Mon2, process, P, noproc} = recv_one(),
    Mon3 = erlang:monitor(process, P, [{alias, explicit_unalias}]),
    {'DOWN', Mon3, process, P, noproc} = recv_one(),
    ok.

spawn_monitor(LoopFun, Opts) ->
    spawn_opt(LoopFun, [{monitor, Opts}]).

spawn_and_monitor(LoopFun, Opts) ->
    P = spawn_opt(LoopFun, []),
    Mon = erlang:monitor(process, P, Opts),
    {P, Mon}.

normal_loop() ->
    receive
        {Caller, quit} -> Caller ! {self(), finished}
    end.

echo_loop() ->
    receive
        quit ->
            ok;
        {Msg, ReplyTo} ->
            ReplyTo ! Msg,
            echo_loop()
    end.

recv_one() ->
    receive
        Msg -> Msg
    after 500 -> timeout
    end.
