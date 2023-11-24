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
    test_clock(system_time, fun() -> erlang:system_time(millisecond) end),
    test_clock(monotonic_time, fun() -> erlang:monotonic_time(millisecond) end),
    Date = erlang:universaltime(),
    if
        Date < ?START_DATE ->
            pico:rtc_set_datetime(?START_DATE);
        true ->
            ok
    end,
    test_clock(system_time_after_set_rtc, fun() -> erlang:system_time(millisecond) end),
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

test_clock(Case, Fun) ->
    StartTime = Fun(),
    receive
    after 100 -> ok
    end,
    EndTime = Fun(),
    Delta = EndTime - StartTime,
    if
        Delta >= 100 andalso Delta < 500 -> ok;
        true -> throw({unexpected_delta, Delta, Case, StartTime, EndTime})
    end.
