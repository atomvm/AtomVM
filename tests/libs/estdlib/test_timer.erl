%
% This file is part of AtomVM.
%
% Copyright 2019-2021 Fred Dushin <fred@dushin.net>
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

-module(test_timer).

-export([test/0]).

test() ->
    ok = test_timer(),
    ok = test_timer_interrupt(),
    ok = test_timer_loop(),
    ok.

-include("etest.hrl").

test_timer() ->
    T0 = erlang:timestamp(),
    ok = timer:sleep(101),
    T1 = erlang:timestamp(),
    ok = etest:assert_true((to_ms(T1) - to_ms(T0)) >= 101),
    ok.

test_timer_interrupt() ->
    Self = self(),
    Pid = spawn(fun() -> do_test_interrupt(Self) end),
    receive ready -> ok end,

    %% this message should not interrupt the timer
    Pid ! try_to_interrupt,

    Pid ! '$atomvm_timer_interrupt',
    ?ASSERT_MATCH(pop_mailbox(), ok),
    ok.

pop_mailbox() ->
    receive
        X -> X
    end.

do_test_interrupt(Pid) ->
    Pid ! ready,
    case timer:sleep(infinity) of
        {error, unexpected_interrupt} ->
            Pid ! ok;
        _ ->
            Pid ! error
    end.

test_timer_loop() ->
    Self = self(),
    spawn(fun() -> timer:sleep(220), Self ! ping end),
    ok = timer_loop(5).

timer_loop(0) ->
    ok;
timer_loop(I) ->
    T0 = erlang:timestamp(),
    ok = timer:sleep(101),
    T1 = erlang:timestamp(),
    ok = etest:assert_true((to_ms(T1) - to_ms(T0)) >= 101),
    timer_loop(I - 1).

to_ms({MegaSecs, Secs, MicroSecs}) ->
    ((MegaSecs * 1000000 + Secs) * 1000 + MicroSecs div 1000).
