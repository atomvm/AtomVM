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

-module(test_bigintegers_ordering).

-export([start/0, sort/1, insert/2, check/1, pow/2]).

%% erlfmt-ignore
start() ->
    Sorted = sort([pow(2, 62), 0, pow(2, 28), -pow(2, 30), 42, pow(2, 27), pow(-2, 31), pow(2, 25),
                   -pow(2, 62)]),
    check(Sorted) +
    bool_to_n(Sorted < [pow(2, 61)]) * 2 +
    bool_to_n(Sorted > {-pow(2, 61)}) * 4.

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

%% erlfmt-ignore
check(T) ->
    Expected = [-pow(2, 62), pow(-2, 31), -pow(2, 30), 0, 42, pow(2, 25), pow(2, 27), pow(2, 28),
                pow(2, 62)],
    check(T, Expected).

pow(_N, 0) ->
    1;
pow(N, M) ->
    N * pow(N, M - 1).

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
