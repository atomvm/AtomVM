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

-module(make_garbage4).

-export([start/0, sort/1, insert/2, check/1]).

%% erlfmt-ignore
start() ->
    check(sort([5, 7, 9, 15, 18, 22, 4, 11, 89, 94, 1, 0, 5, 8, 9, 3, 4, 5, 35, 12, 93, 29, 39, 29,
          22, 93, 23, 28, 98, 32, 32, 42, 91, 83, 38, 18, 98, 10, 12, 39, 14, 12, 93, 32, 23,
          93, 9, 13, 40, 34, 23, 23, 24, 42, 23, 23, 42, 90, 23, 32, 94, 42, 11, 32, 12, 12], [])).

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
