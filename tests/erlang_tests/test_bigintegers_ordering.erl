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
    ok = mixed_numbers(),
    Sorted = sort([pow(2, 62), 0, pow(2, 28), -pow(2, 30), 42, pow(2, 27), pow(-2, 31), pow(2, 25),
                   -pow(2, 62)]),
    check(Sorted) +
    bool_to_n(Sorted < [pow(2, 61)]) * 2 +
    bool_to_n(Sorted > {-pow(2, 61)}) * 4.

mixed_numbers() ->
    ok = mixed_powers([24, 53, 59, 63, 64, 100]),
    ok = less(0, 0.5),
    ok = less(-0.5, 0),
    ok = less(-2, -1.5),
    ok = less(1.5, 2),
    ok = equal(0, 0.0),
    ok = equal(0, -0.0),
    case erlang:system_info(machine) of
        "BEAM" ->
            mixed_powers([127, 255]);
        "ATOM" ->
            case erlang:system_info(avm_floatsize) of
                8 -> mixed_powers([127, 255]);
                4 -> ok
            end
    end.

mixed_powers([]) ->
    ok;
mixed_powers([Power | Rest]) ->
    Integer = 1 bsl Power,
    Float = float(Integer),
    ok = less(Integer - 1, Float),
    ok = less(Float, Integer + 1),
    ok = less(-Integer - 1, -Float),
    ok = less(-Float, -Integer + 1),
    ok = equal(Integer, Float),
    ok = equal(-Integer, -Float),
    %% Equal prefixes must not hide differences in nested terms.
    ok = less({Integer, Integer - 1}, {Float, Float}),
    ok = less(#{value => Float}, #{value => Integer + 1}),
    mixed_powers(Rest).

less(A, B) ->
    {false, true, false, true, true, true, false, false} = comparisons(A, B),
    {false, true, false, true, false, false, true, true} = comparisons(B, A),
    ok = less_guard(A, B).

less_guard(A, B) when A < B, A =< B, B > A, B >= A, A /= B ->
    ok.

equal(A, B) ->
    {true, false, false, true, false, true, false, true} = comparisons(A, B),
    {true, false, false, true, false, true, false, true} = comparisons(B, A),
    ok.

comparisons(A, B) ->
    {A == B, A /= B, A =:= B, A =/= B, A < B, A =< B, A > B, A >= B}.

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
