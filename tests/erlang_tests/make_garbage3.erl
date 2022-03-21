%
% This file is part of AtomVM.
%
% Copyright 2018 Davide Bettio <davide@uninstall.it>
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

-module(make_garbage3).

-export([start/0, sort/1, insert/2, check/1]).

start() ->
    check(sort([7, 1, 5])).

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

check(L) ->
    check(L, 0, 0).

check([], _, Acc) ->
    Acc;
check([H | T], 0, Acc) ->
    check(T, 1, Acc + H);
check([0 | T], 1, Acc) ->
    check(T, 2, Acc * 2);
check([H | T], 1, Acc) ->
    check(T, 2, Acc div H);
check([H | T], 2, Acc) ->
    check(T, 3, Acc - H);
check([0 | T], 3, Acc) ->
    check(T, 0, Acc - 1);
check([H | T], 3, Acc) ->
    check(T, 0, Acc * H).
