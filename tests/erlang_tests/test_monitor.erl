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

normal_loop() ->
    receive
        {Caller, quit} -> Caller ! {self(), finished}
    end.
