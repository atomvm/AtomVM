%
% This file is part of AtomVM.
%
% Copyright 2019 Davide Bettio <davide@uninstall.it>
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

-module(test_pids_ordering).

-export([start/0, sort/1, insert/2, check/2]).

start() ->
    P1 = spawn_opt(?MODULE, sort, [[]], []),
    P2 = spawn_opt(?MODULE, sort, [[1]], []),
    P3 = spawn_opt(?MODULE, sort, [[2, 1]], []),
    P4 = spawn_opt(?MODULE, sort, [[3]], []),
    P5 = spawn_opt(?MODULE, sort, [[4, 1]], []),
    P6 = spawn_opt(?MODULE, sort, [[]], []),
    Sorted = sort([P3, P4, P1, P5, P2]),
    check(Sorted, [P1, P2, P3, P4, P5]) +
        bool_to_n(Sorted < [P6]) * 2 +
        bool_to_n(Sorted > {P6}) * 4.

sort(L) ->
    sort(L, []).

sort([], Sorted) ->
    Sorted;
sort([H | Unsorted], Sorted) ->
    NextSorted = insert(Sorted, H),
    sort(Unsorted, NextSorted).

insert(L, I) ->
    insert(L, [], I).

insert([], HL, I) ->
    HL ++ [I];
insert([H | T], HL, I) when I < H ->
    HL ++ [I, H | T];
insert([H | T], HL, I) ->
    insert(T, HL ++ [H], I).

check(T, Expected) when T == Expected ->
    1;
check(T, Expected) when T /= Expected ->
    0.

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
