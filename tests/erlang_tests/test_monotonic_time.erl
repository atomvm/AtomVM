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

    1.

test_diff(X) when is_integer(X) andalso X >= 0 ->
    1;
test_diff(X) when X < 0 ->
    0.

test_native_monotonic_time() ->
    Na1 = erlang:monotonic_time(native),
    receive
    after 1 -> ok
    end,
    Na2 = erlang:monotonic_time(native),
    true = is_integer(Na2 - Na1) andalso (Na2 - Na1) >= 0,
    ok.
