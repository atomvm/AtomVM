%
% This file is part of AtomVM.
%
% Copyright 2019-2020 Fred Dushin <fred@dushin.net>
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

-module(test_timer_manager).

-export([test/0]).

test() ->
    ok = test_start_timer(),
    ok = test_cancel_timer(),
    ok = test_cancel_timer_after_expiry(),
    ok = test_erlang_cancel_timer(),
    ok = test_timer_badargs(),
    ok = test_send_after_boundaries(),
    lists:foreach(
        fun(Function) ->
            lists:foreach(
                fun(Time) ->
                    ok = test_destination_death(Function, before_creation, Time),
                    ok = test_destination_death(Function, after_creation, Time)
                end,
                [0, 1, 60000]
            ),
            ok = test_named_destination_death(Function)
        end,
        [send_after, start_timer]
    ),
    pong = test_send_after(),
    ok = test_cancel_send_after(),
    ok = test_cancel_send_after_suppresses_message(),
    ok.

-include("etest.hrl").

test_start_timer() ->
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), []),
    timer_manager:start_timer(100, self(), test_start_timer),
    wait_for_timeout(test_start_timer, 5000).

test_cancel_timer() ->
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), []),
    TimerRef = timer_manager:start_timer(60000, self(), test_cancel_timer),
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), [TimerRef]),
    R = timer_manager:cancel_timer(TimerRef),
    ?ASSERT_TRUE(is_integer(R)),
    ?ASSERT_TRUE(R > 0),
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), []),
    R2 = timer_manager:cancel_timer(TimerRef),
    ?ASSERT_EQUALS(false, R2),
    ok.

test_cancel_timer_after_expiry() ->
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), []),
    TimerRef = timer_manager:start_timer(10, self(), test_cancel_after_expiry),
    receive
        {timeout, TimerRef, test_cancel_after_expiry} -> ok
    after 5000 -> throw(timeout)
    end,
    R = timer_manager:cancel_timer(TimerRef),
    ?ASSERT_EQUALS(false, R),
    ok.

test_erlang_cancel_timer() ->
    TimerRef = erlang:start_timer(60000, self(), test_erlang_cancel),
    R = erlang:cancel_timer(TimerRef),
    ?ASSERT_TRUE(is_integer(R)),
    ?ASSERT_TRUE(R > 0),
    R2 = erlang:cancel_timer(TimerRef),
    ?ASSERT_EQUALS(false, R2),
    ok.

test_timer_badargs() ->
    lists:foreach(
        fun(Time) -> ?ASSERT_ERROR(erlang:send_after(Time, self(), ignored), badarg) end,
        [-1, 1.0, bad, [], infinity, 1 bsl 63]
    ),
    Remote = binary_to_term(<<131, 88, 119, 14, "remote@invalid", 1:32, 0:32, 0:32>>),
    lists:foreach(
        fun(Dest) -> ?ASSERT_ERROR(erlang:send_after(60000, Dest, ignored), badarg) end,
        [make_ref(), 0, [], {name, node()}, Remote]
    ),
    lists:foreach(
        fun(Ref) -> ?ASSERT_ERROR(erlang:cancel_timer(Ref), badarg) end,
        [timer, 0, [], self()]
    ),
    ?ASSERT_EQUALS(false, erlang:cancel_timer(make_ref())),
    ok.

test_send_after_boundaries() ->
    Ref = erlang:send_after(0, self(), immediate),
    ?ASSERT_TRUE(is_reference(Ref)),
    receive
        immediate -> ok
    after 5000 -> throw(timeout)
    end,
    ?ASSERT_EQUALS(false, erlang:cancel_timer(Ref)),
    Max = 16#FFFFFFFF,
    LongRef = erlang:send_after(Max, self(), long_timer),
    Remaining = erlang:cancel_timer(LongRef),
    ?ASSERT_TRUE(is_integer(Remaining) andalso Remaining >= 0 andalso Remaining =< Max),
    %% AtomVM's receive timeout, used by the timer worker, is limited to uint32.
    ?ASSERT_ERROR(erlang:send_after(Max + 1, self(), ignored), badarg),
    true = register(test_timer_destination, self()),
    NamedRef = erlang:send_after(0, test_timer_destination, named_timer),
    ?ASSERT_TRUE(is_reference(NamedRef)),
    receive
        named_timer -> ok
    after 5000 -> throw(timeout)
    end,
    true = unregister(test_timer_destination),
    ok.

test_destination_death(Function, When, Time) ->
    {Dest, Monitor} = spawn_monitor(fun() ->
        receive
            stop -> ok
        end
    end),
    case When of
        before_creation -> stop_destination(Dest, Monitor);
        after_creation -> ok
    end,
    Ref = apply(erlang, Function, [Time, Dest, ignored]),
    AutoRef = apply(erlang, Function, [Time, Dest, ignored]),
    case When of
        before_creation -> ok;
        after_creation -> stop_destination(Dest, Monitor)
    end,
    %% Cancel immediately after DOWN: the worker may not have observed the
    %% destination's death yet. It must still report that the timer is gone.
    ?ASSERT_EQUALS(false, erlang:cancel_timer(Ref)),
    ?ASSERT_EQUALS(false, erlang:cancel_timer(Ref)),
    %% The other timer must disappear without any explicit cancellation.
    wait_for_timer_removal(AutoRef, erlang:monotonic_time(millisecond) + 5000).

test_named_destination_death(Function) ->
    {Dest, Monitor} = spawn_monitor(fun() ->
        receive
            stop -> ok
        end
    end),
    true = register(test_timer_destination, Dest),
    Ref = apply(erlang, Function, [60000, test_timer_destination, ignored]),
    stop_destination(Dest, Monitor),
    Remaining = erlang:cancel_timer(Ref),
    ?ASSERT_TRUE(is_integer(Remaining) andalso Remaining > 0 andalso Remaining =< 60000),
    ?ASSERT_EQUALS(false, erlang:cancel_timer(Ref)),
    ok.

stop_destination(Dest, Monitor) ->
    Dest ! stop,
    receive
        {'DOWN', Monitor, process, Dest, normal} -> ok
    after 5000 -> throw(timeout)
    end.

wait_for_timer_removal(Ref, Deadline) ->
    case lists:member(Ref, timer_manager:get_timer_refs()) of
        false ->
            ok;
        true ->
            ?ASSERT_TRUE(erlang:monotonic_time(millisecond) < Deadline),
            receive
            after 1 -> ok
            end,
            wait_for_timer_removal(Ref, Deadline)
    end.

test_send_after() ->
    timer_manager:send_after(100, self(), ping),
    pong = wait_for_timeout(ping, 5000).

test_cancel_send_after() ->
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), []),
    TimerRef = timer_manager:send_after(60000, self(), test_cancel_send_after),
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), [TimerRef]),
    R = timer_manager:cancel_timer(TimerRef),
    ?ASSERT_TRUE(is_integer(R)),
    ?ASSERT_TRUE(R > 0),
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), []),
    R2 = timer_manager:cancel_timer(TimerRef),
    ?ASSERT_EQUALS(false, R2),
    ok.

test_cancel_send_after_suppresses_message() ->
    ?ASSERT_MATCH(timer_manager:get_timer_refs(), []),
    TimerRef = timer_manager:send_after(100, self(), should_not_arrive),
    receive
    after 50 -> ok
    end,
    _ = timer_manager:cancel_timer(TimerRef),
    receive
        should_not_arrive -> throw(timer_fired_despite_cancel)
    after 500 -> ok
    end,
    ok.

wait_for_timeout(Msg, Timeout) ->
    receive
        {timeout, _TimerRef, Msg} ->
            ok;
        Msg ->
            pong
    after Timeout -> fail
    end.
