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

-module(test_clocks).
-export([start/0]).

-define(START_DATE, {{2023, 9, 3}, {21, 30, 00}}).
-define(BEFORE_START_DATE, 1693769340).

start() ->
    test_clock(system_time, fun() -> erlang:system_time(millisecond) end, 100, 500),
    test_clock(monotonic_time, fun() -> erlang:monotonic_time(millisecond) end, 100, 500),
    test_clock(
        system_time_nanosecond, fun() -> erlang:system_time(nanosecond) end, 100000000, 500000000
    ),
    test_clock(
        monotonic_time_nanosecond,
        fun() -> erlang:monotonic_time(nanosecond) end,
        100000000,
        500000000
    ),
    test_clock(system_time_native, fun() -> erlang:system_time(native) end, 100000000, 500000000),
    test_clock(
        monotonic_time_native, fun() -> erlang:monotonic_time(native) end, 100000000, 500000000
    ),
    Date = erlang:universaltime(),
    if
        Date < ?START_DATE ->
            pico:rtc_set_datetime(?START_DATE);
        true ->
            ok
    end,
    ok = test_time_unit_ratios(system_time, fun(Unit) -> erlang:system_time(Unit) end),
    ok = test_time_unit_ratios(monotonic_time, fun(Unit) -> erlang:monotonic_time(Unit) end),
    test_clock(system_time_after_set_rtc, fun() -> erlang:system_time(millisecond) end, 100, 500),
    NewDate = erlang:universaltime(),
    if
        NewDate >= ?START_DATE -> ok;
        true -> throw({unexpected_date, NewDate})
    end,
    atomvm:posix_clock_settime(realtime, {?BEFORE_START_DATE, 0}),
    PostSetDate = erlang:universaltime(),
    if
        PostSetDate >= ?START_DATE ->
            throw({unexpected_date, NewDate});
        true ->
            ok
    end,
    ok.

% Readings coarsest-to-finest: finer value is always taken later so
% finer >= coarser * scale.  Upper bound allows 1 ms between calls.
test_time_unit_ratios(Case, Fun) ->
    S = Fun(second),
    Ms = Fun(millisecond),
    Us = Fun(microsecond),
    Ns = Fun(nanosecond),

    Ms >= S * 1000 orelse throw({ratio_fail, Case, second_to_ms, S, Ms}),
    Ms < S * 1000 + 1000 orelse throw({ratio_upper, Case, second_to_ms, S, Ms}),

    Us >= Ms * 1000 orelse throw({ratio_fail, Case, ms_to_us, Ms, Us}),
    Us < Ms * 1000 + 1000000 orelse throw({ratio_upper, Case, ms_to_us, Ms, Us}),

    Ns >= Us * 1000 orelse throw({ratio_fail, Case, us_to_ns, Us, Ns}),
    Ns < Us * 1000 + 1000000 orelse throw({ratio_upper, Case, us_to_ns, Us, Ns}),

    ok.

test_clock(Case, Fun, MinDelta, MaxDelta) ->
    StartTime = Fun(),
    receive
    after 100 -> ok
    end,
    EndTime = Fun(),
    Delta = EndTime - StartTime,
    if
        Delta >= MinDelta andalso Delta < MaxDelta -> ok;
        true -> throw({unexpected_delta, Delta, Case, StartTime, EndTime})
    end.
