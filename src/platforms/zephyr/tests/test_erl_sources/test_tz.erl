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

-module(test_tz).
-export([start/0]).

%% Slack to absorb unrelated allocator noise / atom table growth between
%% Before/After snapshots. The setenv leak on newlib was ~strlen("TZ=" ++ Rule)
%% bytes per call, so 500 iterations would add up to tens of KB if the leak
%% were still present. 1024 bytes of slack is well below that.
-define(LEAK_SLACK_BYTES, 1024).
-define(LEAK_ITERATIONS, 500).

start() ->
    try
        ok = test_basic(),
        ok = test_offsets(),
        ok = test_localtime0_after_localtime1(),
        ok = test_badarg(),
        ok = test_no_leak(),
        ok
    catch
        Class:Reason:Stack ->
            erlang:display({Class, Reason, Stack}),
            error
    end.

%% Original sanity check: localtime("UTC") should fall between two
%% surrounding universaltime() snapshots.
test_basic() ->
    UTCTime1 = erlang:universaltime(),
    LocalTimeUTC = erlang:localtime("UTC"),
    UTCTime2 = erlang:universaltime(),
    true = UTCTime1 =< LocalTimeUTC andalso LocalTimeUTC =< UTCTime2,
    ParisTime = erlang:localtime("CET-1CEST,M3.5.0,M10.5.0/3"),
    UTCTime3 = erlang:universaltime(),
    true = UTCTime2 =/= ParisTime andalso UTCTime3 =/= ParisTime,
    ok.

%% Verify that several POSIX TZ strings produce the expected offsets
%% relative to UTC. We capture three localtime/1 calls back-to-back and
%% allow a small drift for the wall clock advancing between calls.
test_offsets() ->
    UTC = erlang:localtime("UTC0"),
    %% UTC-5, no DST
    EST = erlang:localtime("EST5"),
    %% UTC+9, no DST
    JST = erlang:localtime("JST-9"),

    UTCSecs = calendar:datetime_to_gregorian_seconds(UTC),
    ESTSecs = calendar:datetime_to_gregorian_seconds(EST),
    JSTSecs = calendar:datetime_to_gregorian_seconds(JST),

    %% EST should read ~5h earlier than UTC; JST ~9h later than UTC.
    %% Allow 5s drift for clock advance between the three calls.
    DiffEST = UTCSecs - ESTSecs,
    DiffJST = JSTSecs - UTCSecs,
    true = abs(DiffEST - 5 * 3600) =< 5,
    true = abs(DiffJST - 9 * 3600) =< 5,

    %% Strict ordering: EST < UTC < JST (allowing equality due to drift).
    true = ESTSecs =< UTCSecs,
    true = UTCSecs =< JSTSecs,
    ok.

%% After many TZ overrides the global libc state must be back to its
%% original timezone -- localtime/0 should still match universaltime/0
%% (assuming the device boots with TZ unset / UTC).
test_localtime0_after_localtime1() ->
    _ = erlang:localtime("EST5EDT,M3.2.0/2,M11.1.0/2"),
    _ = erlang:localtime("CET-1CEST,M3.5.0,M10.5.0/3"),
    _ = erlang:localtime("JST-9"),
    UTC1 = erlang:universaltime(),
    Local = erlang:localtime(),
    UTC2 = erlang:universaltime(),
    %% Default TZ on ESP32 is UTC, so localtime/0 should equal universaltime/0
    %% modulo the second tick.
    true = UTC1 =< Local andalso Local =< UTC2,
    ok.

%% A non-string / non-list / non-binary TZ argument must raise badarg, not
%% crash or leak. Binaries are accepted by interop_term_to_string and are
%% therefore valid TZ inputs (verified below).
test_badarg() ->
    ok = expect_badarg(fun() -> erlang:localtime(42) end),
    ok = expect_badarg(fun() -> erlang:localtime({tuple, ok}) end),
    %% Binary TZ should be accepted (and decoded).
    UTCTime1 = erlang:universaltime(),
    BinUTC = erlang:localtime(<<"UTC0">>),
    UTCTime2 = erlang:universaltime(),
    true = UTCTime1 =< BinUTC andalso BinUTC =< UTCTime2,
    ok.

expect_badarg(F) ->
    try F() of
        R -> {unexpected_result, R}
    catch
        error:badarg -> ok
    end.

%% Memory-leak regression test for the newlib/picolibc setenv leak fix.
%% Calls localtime/1 ?LEAK_ITERATIONS times rotating through 4 distinct
%% TZ rules and asserts the free heap does not shrink by more than
%% ?LEAK_SLACK_BYTES.
test_no_leak() ->
    %% Warm-up: trigger any one-time allocations (atom table growth,
    %% libc internal state, etc.) before snapshotting the heap.
    leak_loop(16),
    erlang:garbage_collect(),

    case erlang:system_info(esp32_free_heap_size) of
        undefined ->
            erlang:display({tz_leak_check, skipped_not_supported}),
            ok;
        Before ->
            leak_loop(?LEAK_ITERATIONS),
            erlang:garbage_collect(),
            After = erlang:system_info(esp32_free_heap_size),

            Delta = Before - After,
            erlang:display({tz_leak_check, before, Before, after_, After, delta, Delta}),

            case Delta =< ?LEAK_SLACK_BYTES of
                true ->
                    ok;
                false ->
                    erlang:display({tz_leak_detected, Delta, slack, ?LEAK_SLACK_BYTES}),
                    {error, {tz_leak_detected, Delta}}
            end
    end.

leak_loop(0) ->
    ok;
leak_loop(N) ->
    TZ =
        case N rem 4 of
            0 -> "UTC0";
            1 -> "EST5EDT,M3.2.0/2,M11.1.0/2";
            2 -> "CET-1CEST,M3.5.0,M10.5.0/3";
            3 -> "JST-9"
        end,
    _ = erlang:localtime(TZ),
    leak_loop(N - 1).
