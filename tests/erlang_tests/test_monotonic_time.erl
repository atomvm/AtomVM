%
% This file is part of AtomVM.
%
% Copyright 2023 Davide Bettio <davide@uninstall.it>
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

-module(test_monotonic_time).

-export([start/0]).

start() ->
    T1 = erlang:monotonic_time(millisecond),
    receive
    after 1 -> ok
    end,
    T2 = erlang:monotonic_time(millisecond),
    1 = test_diff(T2 - T1),

    N1 = erlang:monotonic_time(nanosecond),
    receive
    after 1 -> ok
    end,
    N2 = erlang:monotonic_time(nanosecond),
    true = is_integer(N2 - N1) andalso (N2 - N1) >= 0,

    ok = test_native_monotonic_time(),
    ok = test_integer_time_unit(),
    ok = test_bad_integer_time_unit(),

    1.

test_diff(X) when is_integer(X) andalso X >= 0 ->
    1;
test_diff(X) when X < 0 ->
    0.

expect(F, Expect) ->
    try
        F(),
        fail
    catch
        _:E when E == Expect ->
            ok
    end.

test_native_monotonic_time() ->
    Na1 = erlang:monotonic_time(native),
    receive
    after 1 -> ok
    end,
    Na2 = erlang:monotonic_time(native),
    true = is_integer(Na2 - Na1) andalso (Na2 - Na1) >= 0,
    ok.

test_integer_time_unit() ->
    %% integer 1 = parts per second, equivalent to second
    S = erlang:monotonic_time(second),
    S1 = erlang:monotonic_time(1),
    true = abs(S1 - S) =< 1,

    %% integer 1000 = parts per second, equivalent to millisecond
    Ms = erlang:monotonic_time(millisecond),
    Ms1 = erlang:monotonic_time(1000),
    true = abs(Ms1 - Ms) =< 1,

    %% integer 1000000 = parts per second, equivalent to microsecond
    Us = erlang:monotonic_time(microsecond),
    Us1 = erlang:monotonic_time(1000000),
    true = abs(Us1 - Us) =< 1000,

    %% integer 1000000000 = parts per second, equivalent to nanosecond
    Ns = erlang:monotonic_time(nanosecond),
    Ns1 = erlang:monotonic_time(1000000000),
    true = abs(Ns1 - Ns) =< 1000000,

    %% verify monotonicity with integer unit
    T1 = erlang:monotonic_time(1000),
    receive
    after 1 -> ok
    end,
    T2 = erlang:monotonic_time(1000),
    true = T2 >= T1,

    ok.

test_bad_integer_time_unit() ->
    ok = expect(fun() -> erlang:monotonic_time(0) end, badarg),
    ok = expect(fun() -> erlang:monotonic_time(-1) end, badarg),
    ok.
