%
% This file is part of AtomVM.
%
% Copyright 2018-2023 Davide Bettio <davide@uninstall.it>
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

-module(test_system_time).

-export([start/0]).

start() ->
    ok = test_system_time(second, 1001),
    ok = test_system_time(millisecond, 10),
    ok = test_system_time(microsecond, 1),
    ok = test_nanosecond_system_time(),
    ok = test_native_system_time(),

    ok = test_erlang_monotonic_time_0(),
    ok = test_erlang_system_time_0(),
    ok = test_os_system_time(),
    ok = test_time_unit_ratios(),

    ok = test_integer_time_unit(),
    ok = test_bad_integer_time_unit(),

    ok = expect(fun() -> erlang:system_time(not_a_time_unit) end, badarg),

    ok = test_system_time_to_universal_time(),
    ok = test_integer_unit_universal_time(),
    ok = test_bad_integer_unit_universal_time(),

    0.

test_system_time(Unit, SleepMs) ->
    Before = verify_system_time_value(erlang:system_time(Unit)),
    sleep(SleepMs),
    After = verify_system_time_value(erlang:system_time(Unit)),
    true = (After > Before),
    ok.

test_erlang_monotonic_time_0() ->
    T = erlang:monotonic_time(),
    true = is_integer(T),
    ok.

test_erlang_system_time_0() ->
    T = erlang:system_time(),
    true = is_integer(T) andalso T > 0,
    ok.

test_os_system_time() ->
    T0 = os:system_time(),
    true = is_integer(T0) andalso T0 > 0,
    ok = test_system_time_unit(os_system_time_second, fun() -> os:system_time(second) end),
    ok = test_system_time_unit(os_system_time_millisecond, fun() -> os:system_time(millisecond) end),
    ok = test_system_time_unit(os_system_time_microsecond, fun() -> os:system_time(microsecond) end),
    ok = test_os_system_time_native(),
    ok.

test_system_time_unit(_Name, Fun) ->
    T = Fun(),
    true = is_integer(T) andalso T > 0,
    ok.

% Readings are taken coarsest-to-finest so the finer value is always >= the
% coarser value * its scale factor.  The upper-bound allows for at most 1 s
% of wall-clock time between consecutive calls.
test_time_unit_ratios() ->
    S = erlang:system_time(second),
    Ms = erlang:system_time(millisecond),
    Us = erlang:system_time(microsecond),

    true = Ms >= S * 1000,
    % within 1 s
    true = Ms < S * 1000 + 1000,

    true = Us >= Ms * 1000,
    % within 1 s
    true = Us < Ms * 1000 + 1000000,

    ok = test_ns_ratio(),

    ok.

verify_system_time_value(M) when is_integer(M) andalso M > 0 ->
    M.

sleep(Ms) ->
    receive
    after Ms ->
        ok
    end.

expect(F, Expect) ->
    try
        F(),
        fail
    catch
        _:E when E == Expect ->
            ok
    end.

test_system_time_to_universal_time() ->
    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(0, second),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1, second),

    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(0, millisecond),
    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(1, millisecond),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1000, millisecond),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1001, millisecond),

    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(0, microsecond),
    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(1, microsecond),
    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(1000, microsecond),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1000000, microsecond),

    {{2023, 7, 8}, {20, 19, 39}} = calendar:system_time_to_universal_time(1688847579, second),

    {{1969, 12, 31}, {23, 59, 59}} = calendar:system_time_to_universal_time(-1, second),

    ok = test_nanosecond_universal_time(),
    ok = test_native_universal_time(),

    ok.

test_nanosecond_system_time() ->
    ok = test_system_time(nanosecond, 1).

test_native_system_time() ->
    ok = test_system_time(native, 1).

test_os_system_time_native() ->
    ok = test_system_time_unit(os_system_time_nanosecond, fun() -> os:system_time(nanosecond) end),
    ok = test_system_time_unit(os_system_time_native, fun() -> os:system_time(native) end),
    ok.

test_ns_ratio() ->
    Us = erlang:system_time(microsecond),
    Ns = erlang:system_time(nanosecond),
    true = Ns >= Us * 1000,
    % within 1 ms
    true = Ns < Us * 1000 + 1000000,
    ok.

test_nanosecond_universal_time() ->
    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(0, nanosecond),
    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(1, nanosecond),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1000000000, nanosecond),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1001000000, nanosecond),
    {{2023, 7, 8}, {20, 19, 39}} = calendar:system_time_to_universal_time(
        1688847579000000000, nanosecond
    ),
    ok.

test_native_universal_time() ->
    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(0, native),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1000000000, native),
    ok.

test_integer_time_unit() ->
    %% integer 1 = parts per second, equivalent to second
    S = erlang:system_time(second),
    S1 = erlang:system_time(1),
    true = abs(S1 - S) =< 1,

    %% integer 1000 = parts per second, equivalent to millisecond
    Ms = erlang:system_time(millisecond),
    Ms1 = erlang:system_time(1000),
    true = abs(Ms1 - Ms) =< 1,

    %% integer 1000000 = parts per second, equivalent to microsecond
    Us = erlang:system_time(microsecond),
    Us1 = erlang:system_time(1000000),
    true = abs(Us1 - Us) =< 1000,

    %% integer 1000000000 = parts per second, equivalent to nanosecond
    Ns = erlang:system_time(nanosecond),
    Ns1 = erlang:system_time(1000000000),
    true = abs(Ns1 - Ns) =< 1000000,

    %% verify values are positive
    true = S1 > 0,
    true = Ms1 > 0,
    true = Us1 > 0,
    true = Ns1 > 0,

    ok.

test_integer_unit_universal_time() ->
    %% integer 1 = seconds
    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(0, 1),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1, 1),
    {{2023, 7, 8}, {20, 19, 39}} = calendar:system_time_to_universal_time(1688847579, 1),

    %% integer 1000 = milliseconds
    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(0, 1000),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1000, 1000),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1001, 1000),
    {{1969, 12, 31}, {23, 59, 59}} = calendar:system_time_to_universal_time(-1, 1000),

    %% integer 1000000 = microseconds
    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(0, 1000000),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1000000, 1000000),
    {{1969, 12, 31}, {23, 59, 59}} = calendar:system_time_to_universal_time(-1, 1000000),

    ok.

test_bad_integer_time_unit() ->
    ok = expect(fun() -> erlang:system_time(0) end, badarg),
    ok = expect(fun() -> erlang:system_time(-1) end, badarg),
    ok.

test_bad_integer_unit_universal_time() ->
    ok = expect(fun() -> calendar:system_time_to_universal_time(0, 0) end, badarg),
    ok.
