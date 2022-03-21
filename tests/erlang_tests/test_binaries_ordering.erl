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

-module(test_binaries_ordering).

-export([start/0, sort/1, insert/2, check/1]).

start() ->
    Sorted = sort([<<"Foo">>, <<"Bar">>, <<"Alice">>, <<"Bob">>, <<"lowercase">>]),
    Sorted2 = sort([
        <<"aaa">>,
        <<"aa">>,
        <<"aaz">>,
        <<"b">>,
        <<"a">>,
        <<"z">>,
        <<"c">>,
        <<"d">>,
        <<"dd">>,
        <<"test_binaries_ordering">>,
        <<"start">>,
        <<"sort">>,
        <<"erlang">>
    ]),
    check(Sorted) +
        bool_to_n(Sorted > [<<"Aalice">>]) * 2 +
        bool_to_n(Sorted < [<<"Bar">>]) * 4 +
        check(Sorted2).

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

check(T) when
    T ==
        [
            <<"a">>,
            <<"aa">>,
            <<"aaa">>,
            <<"aaz">>,
            <<"b">>,
            <<"c">>,
            <<"d">>,
            <<"dd">>,
            <<"erlang">>,
            <<"sort">>,
            <<"start">>,
            <<"test_binaries_ordering">>,
            <<"z">>
        ]
->
    8;
check(T) when T == [<<"Alice">>, <<"Bar">>, <<"Bob">>, <<"Foo">>, <<"lowercase">>] ->
    1;
check(_T) ->
    0.

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
