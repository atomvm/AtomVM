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
    pong = test_send_after(),
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

test_send_after() ->
    timer_manager:send_after(100, self(), ping),
    pong = wait_for_timeout(ping, 5000).

wait_for_timeout(Msg, Timeout) ->
    receive
        {timeout, _TimerRef, Msg} ->
            ok;
        Msg ->
            pong
    after Timeout -> fail
    end.
