%
% This file is part of AtomVM.
%
% Copyright 2018 Davide Bettio <davide@uninstall.it>
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

-module(sleep).

-export([start/0]).

% The only contract of timers is that it is at least a given time
% We check it works by spawning several processes
start() ->
    ok = test_main_sleep(),
    ok = test_four_sleeping_processes(),
    0.

test_main_sleep() ->
    sub_sleep(242),
    ok.

test_four_sleeping_processes() ->
    Pids = spawn_sleeping_processes([182, 242, 202, 222], []),
    wait_for_sleeping_processes(Pids),
    ok.

spawn_sleeping_processes([], Pids) ->
    Pids;
spawn_sleeping_processes([Timeout | T], Acc) ->
    Parent = self(),
    Pid = spawn_opt(
        fun() ->
            ok = sub_sleep(Timeout),
            Parent ! {self(), ok}
        end,
        []
    ),
    spawn_sleeping_processes(T, [Pid | Acc]).

wait_for_sleeping_processes([]) ->
    ok;
wait_for_sleeping_processes([Pid | T]) ->
    ok =
        receive
            {Pid, ok} -> ok
        after 1000 -> {fail, timeout}
        end,
    wait_for_sleeping_processes(T).

sub_sleep(T) ->
    Before = erlang:monotonic_time(millisecond),
    receive
    after T -> ok
    end,
    After = erlang:monotonic_time(millisecond),
    ok =
        if
            After - Before >= T ->
                ok;
            true ->
                {fail, Before, After, T}
        end,
    ok.
