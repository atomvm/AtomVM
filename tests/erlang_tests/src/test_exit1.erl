%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_exit1).

-export([start/0]).

start() ->
    ok = test_catch(),
    ok = test_nocatch(),
    ok = test_trap_kill(),
    0.

test_catch() ->
    ok =
        try
            exit(normal),
            fail
        catch
            exit:normal -> ok
        end,
    ok =
        try
            exit(foo),
            fail
        catch
            exit:foo -> ok
        end,
    ok =
        try
            exit(kill),
            fail
        catch
            exit:kill -> ok
        end,
    ok.

test_nocatch() ->
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            exit(normal)
        end,
        [monitor]
    ),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, normal} -> ok;
            Other -> Other
        after 500 -> timeout
        end,
    {Pid2, Ref2} = spawn_opt(
        fun() ->
            exit(foo)
        end,
        [monitor]
    ),
    ok =
        receive
            {'DOWN', Ref2, process, Pid2, foo} -> ok
        after 500 -> timeout
        end,
    {Pid3, Ref3} = spawn_opt(
        fun() ->
            exit(kill)
        end,
        [monitor]
    ),
    ok =
        receive
            {'DOWN', Ref3, process, Pid3, kill} -> ok
        after 500 -> timeout
        end,
    {Pid4, Ref4} = spawn_opt(
        fun() ->
            process_flag(trap_exit, true),
            exit(normal)
        end,
        [monitor]
    ),
    ok =
        receive
            {'DOWN', Ref4, process, Pid4, normal} -> ok
        after 500 -> timeout
        end,
    {Pid5, Ref5} = spawn_opt(
        fun() ->
            process_flag(trap_exit, true),
            exit(foo)
        end,
        [monitor]
    ),
    ok =
        receive
            {'DOWN', Ref5, process, Pid5, foo} -> ok
        after 500 -> timeout
        end,
    {Pid6, Ref6} = spawn_opt(
        fun() ->
            process_flag(trap_exit, true),
            exit(kill)
        end,
        [monitor]
    ),
    ok =
        receive
            {'DOWN', Ref6, process, Pid6, kill} -> ok
        after 500 -> timeout
        end,
    ok.

test_trap_kill() ->
    process_flag(trap_exit, true),
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            exit(kill)
        end,
        [monitor, link]
    ),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, kill} -> ok
        after 500 -> timeout
        end,
    ok =
        receive
            {'EXIT', Pid1, kill} -> ok
        after 500 -> timeout
        end,
    {Pid2, Ref2} = spawn_opt(
        fun() ->
            spawn_opt(
                fun() ->
                    exit(kill)
                end,
                [link]
            ),
            % wait to be killed by link
            ok =
                receive
                after 500 -> timeout
                end
        end,
        [monitor]
    ),
    ok =
        receive
            {'DOWN', Ref2, process, Pid2, kill} -> ok
        after 500 -> timeout
        end,
    ok.
