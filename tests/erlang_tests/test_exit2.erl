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

-module(test_exit2).

-export([start/0]).

start() ->
    ok = test_reason_other(),
    ok = test_reason_kill(),
    ok = test_reason_normal_not_self(),
    ok = test_reason_normal_self(),
    ok = test_exit_dead(),
    0.

test_reason_other() ->
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            receive
            after 500 -> ok
            end
        end,
        [monitor]
    ),
    true = exit(Pid1, other),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, other} -> ok
        after 500 -> timeout
        end,
    Parent = self(),
    Pid2 = spawn_opt(
        fun() ->
            process_flag(trap_exit, true),
            Parent ! {self(), ready},
            ok =
                receive
                    {'EXIT', _From, _Reason} = ExitMessage ->
                        Parent ! {self(), ExitMessage},
                        ok;
                    Other ->
                        Parent ! {self(), {unexpected, Other}}
                end
        end,
        []
    ),
    ok =
        receive
            {Pid2, ready} -> ok
        after 500 -> timeout
        end,
    true = exit(Pid2, other),
    ok =
        receive
            {Pid2, {'EXIT', Parent, other}} -> ok;
            Other -> Other
        after 500 -> timeout
        end,
    ok.

test_reason_kill() ->
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            receive
            after 500 -> ok
            end
        end,
        [monitor]
    ),
    true = exit(Pid1, kill),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, killed} -> ok
        after 500 -> timeout
        end,
    Parent = self(),
    {Pid2, Ref2} = spawn_opt(
        fun() ->
            process_flag(trap_exit, true),
            Parent ! {self(), ready},
            receive
            after 500 -> ok
            end
        end,
        [monitor]
    ),
    {Pid3, Ref3} = spawn_opt(
        fun() ->
            link(Pid2),
            Parent ! {self(), ready},
            receive
            after 500 -> ok
            end
        end,
        [monitor]
    ),
    {Pid4, Ref4} = spawn_opt(
        fun() ->
            link(Pid2),
            process_flag(trap_exit, true),
            Parent ! {self(), ready},
            receive
                {'EXIT', _From, _Reason} = ExitMessage -> Parent ! {self(), ExitMessage}
            after 500 -> ok
            end
        end,
        [monitor]
    ),
    ok =
        receive
            {Pid2, ready} -> ok
        after 500 -> timeout
        end,
    ok =
        receive
            {Pid3, ready} -> ok
        after 500 -> timeout
        end,
    ok =
        receive
            {Pid4, ready} -> ok
        after 500 -> timeout
        end,
    true = exit(Pid2, kill),
    ok =
        receive
            {'DOWN', Ref2, process, Pid2, killed} -> ok
        after 500 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref3, process, Pid3, killed} -> ok
        after 500 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref4, process, Pid4, normal} -> ok
        after 500 -> timeout
        end,
    Pid2 =
        receive
            {Pid4, {'EXIT', Pid, killed}} -> Pid
        after 500 -> timeout
        end,
    ok.

test_reason_normal_not_self() ->
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            receive
                {Caller, ping} -> Caller ! {self(), pong}
            after 500 -> ok
            end
        end,
        [monitor]
    ),
    true = exit(Pid1, normal),
    Pid1 ! {self(), ping},
    ok =
        receive
            {Pid1, pong} -> ok
        after 500 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, normal} -> ok
        after 500 -> timeout
        end,
    Parent = self(),
    Pid2 = spawn_opt(
        fun() ->
            process_flag(trap_exit, true),
            Parent ! {self(), ready},
            ok =
                receive
                    {'EXIT', _From, _Reason} = ExitMessage ->
                        Parent ! {self(), ExitMessage},
                        ok;
                    Other ->
                        Parent ! {self(), {unexpected, Other}}
                end
        end,
        []
    ),
    ok =
        receive
            {Pid2, ready} -> ok
        after 500 -> timeout
        end,
    true = exit(Pid2, normal),
    ok =
        receive
            {Pid2, {'EXIT', Parent, normal}} -> ok;
            Other -> Other
        after 500 -> timeout
        end,
    ok.

test_reason_normal_self() ->
    Parent = self(),
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            exit(self(), normal),
            Parent ! {self(), unexpected},
            exit(unexpected)
        end,
        [monitor]
    ),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, normal} -> ok
        after 500 -> timeout
        end,
    {Pid2, Ref2} = spawn_opt(
        fun() ->
            process_flag(trap_exit, true),
            exit(self(), normal),
            ok =
                receive
                    {'EXIT', _From, _Reason} = ExitMessage ->
                        Parent ! {self(), ExitMessage},
                        ok;
                    Other ->
                        Parent ! {self(), {unexpected, Other}}
                end,
            exit(expected)
        end,
        [monitor]
    ),
    ok =
        receive
            {Pid2, {'EXIT', Pid2, normal}} -> ok;
            Other -> Other
        after 1000 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref2, process, Pid2, expected} -> ok
        after 500 -> timeout
        end,
    ok.

test_exit_dead() ->
    {Pid1, Ref1} = spawn_opt(fun() -> ok end, [monitor]),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, normal} -> ok
        after 500 -> timeout
        end,
    true = exit(Pid1, normal),
    true = exit(Pid1, kill),
    true = exit(Pid1, other),
    ok.
