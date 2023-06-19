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
    ok = test_monotonic_time(),
    ok.

test_monotonic_time() ->
    test_monotonic_time([500, 1000, 1500, 2500, 5000]).

test_monotonic_time([]) ->
    ok;
test_monotonic_time([Delay | Tail]) ->
    BeginMonotonic = erlang:monotonic_time(millisecond),
    BeginSystem = erlang:system_time(millisecond),
    timer:sleep(Delay),
    EndMonotonic = erlang:monotonic_time(millisecond),
    EndSystem = erlang:system_time(millisecond),
    Mono = EndMonotonic - BeginMonotonic,
    System = EndSystem - BeginSystem,
    true = Mono >= Delay,
    true = Mono + 10 > System,
    true = System + 10 > Mono,
    true = System + 10 > Delay,
    test_monotonic_time(Tail).
