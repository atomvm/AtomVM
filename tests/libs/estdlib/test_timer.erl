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

-include("etest.hrl").

test() ->
    ok = test_timer(),
    ok = test_timer_loop(),
    ok = test_timer_badargs(),
    ok = test_infinity(),
    ok = test_send_after_2(),
    ok = test_send_after_3(),
    ok = test_apply_after(),
    ok.

test_timer() ->
    T0 = erlang:monotonic_time(millisecond),
    ok = timer:sleep(101),
    T1 = erlang:monotonic_time(millisecond),
    ok = etest:assert_true((T1 - T0) >= 101),
    ok.

test_timer_loop() ->
    Self = self(),
    spawn(fun() ->
        Self ! ready,
        timer:sleep(50),
        Self ! noise
    end),
    receive
        ready ->
            ok
    end,
    ok = timer_loop(5).

timer_loop(0) ->
    receive
        noise ->
            ok;
        SomethingElse ->
            {error, SomethingElse}
    end;
timer_loop(I) ->
    T0 = erlang:monotonic_time(millisecond),
    ok = timer:sleep(101),
    T1 = erlang:monotonic_time(millisecond),
    ok = etest:assert_true((T1 - T0) >= 101),
    timer_loop(I - 1).

test_timer_badargs() ->
    {'EXIT', {timeout_value, _}} = (catch timer:sleep(-1)),
    {'EXIT', {timeout_value, _}} = (catch timer:sleep(not_infinity)),
    ok.

test_infinity() ->
    Self = self(),
    Pid = spawn(fun() ->
        Self ! ok,
        timer:sleep(infinity)
    end),
    receive
        ok ->
            ok = etest:assert_true(erlang:is_process_alive(Pid))
    end.

test_send_after_2() ->
    {ok, _TRef} = timer:send_after(500, hello),
    receive
        hello -> {error, too_early}
    after 250 ->
        ok
    end,
    receive
        hello -> ok
    after 10000 ->
        {error, timeout}
    end.

test_send_after_3() ->
    Self = self(),
    {ok, _TRef} = timer:send_after(500, Self, hello_dest),
    receive
        hello_dest -> {error, too_early}
    after 250 ->
        ok
    end,
    receive
        hello_dest -> ok
    after 10000 ->
        {error, timeout}
    end.

test_apply_after() ->
    Self = self(),
    {ok, _TRef} = timer:apply_after(500, erlang, send, [Self, apply_result]),
    receive
        apply_result -> {error, too_early}
    after 250 ->
        ok
    end,
    receive
        apply_result -> ok
    after 10000 ->
        {error, timeout}
    end.
