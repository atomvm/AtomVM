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
    ok = test_system_time(nanosecond, 1),
    ok = test_native_system_time(),

    ok = test_time_unit_ratios(),

    ok = expect(fun() -> erlang:system_time(not_a_time_unit) end, badarg),

    ok = test_system_time_to_universal_time(),

    0.

test_system_time(Unit, SleepMs) ->
    Before = verify_system_time_value(erlang:system_time(Unit)),
    sleep(SleepMs),
    After = verify_system_time_value(erlang:system_time(Unit)),
    true = (After > Before),
    ok.

test_system_time_unit(_Name, Fun) ->
    T = Fun(),
    true = is_integer(T) andalso T > 0,
    ok.

% Readings are taken coarsest-to-finest so the finer value is always >= the
% coarser value * its scale factor.  The upper-bound allows for at most 1 ms
% of wall-clock time between consecutive calls.
test_time_unit_ratios() ->
    S = erlang:system_time(second),
    Ms = erlang:system_time(millisecond),
    Us = erlang:system_time(microsecond),
    Ns = erlang:system_time(nanosecond),

    true = Ms >= S * 1000,
    % within 1 s of each other
    true = Ms < S * 1000 + 1000,

    true = Us >= Ms * 1000,
    % within 1 ms
    true = Us < Ms * 1000 + 1000000,

    true = Ns >= Us * 1000,
    % within 1 ms
    true = Ns < Us * 1000 + 1000000,

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
    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(0, millisecond),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1000, millisecond),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1001, millisecond),

    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(0, microsecond),
    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(1, microsecond),
    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(1000, microsecond),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1000000, microsecond),

    {{2023, 7, 8}, {20, 19, 39}} = calendar:system_time_to_universal_time(1688847579, second),

    {{1969, 12, 31}, {23, 59, 59}} = calendar:system_time_to_universal_time(-1, second),

    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(0, nanosecond),
    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(1, nanosecond),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1000000000, nanosecond),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1001000000, nanosecond),
    {{2023, 7, 8}, {20, 19, 39}} = calendar:system_time_to_universal_time(
        1688847579000000000, nanosecond
    ),

    ok = test_native_universal_time(),

    ok.

-if(?OTP_RELEASE >= 22).
test_native_system_time() ->
    ok = test_system_time(native, 1).

test_native_universal_time() ->
    {{1970, 1, 1}, {0, 0, 0}} = calendar:system_time_to_universal_time(0, native),
    {{1970, 1, 1}, {0, 0, 1}} = calendar:system_time_to_universal_time(1000000000, native),
    ok.
-else.
test_native_system_time() -> ok.
test_native_universal_time() -> ok.
-endif.
