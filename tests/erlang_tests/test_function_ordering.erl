%
% This file is part of AtomVM.
%
% Copyright 2026 AtomVM Contributors
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

-module(test_function_ordering).

-export([start/0, id/1]).

start() ->
    Self = self(),
    Self2 = ?MODULE:id(self()),
    Five = ?MODULE:id(5),
    Six = ?MODULE:id(6),
    F1 = fun(X, Y) -> X + Y end,
    F2 = fun(X, Y) -> X + Y end,
    F3 = fun(X) -> X + Five + Six end,
    % Increasing the last byte of the binary representation
    % effectively increases the last value of the closure,
    % which is the value of variable Six. Thus, F4 only differs
    % form F3 by the last element of the closure.
    F4 = erlang:binary_to_term(increase_last_byte(erlang:term_to_binary(F3))),
    F5 = fun() -> Self ! hello end,
    F6 = fun() -> Self ! Self2 end,
    F7 = fun() -> Self ! Five end,
    F8 = fun(X) -> Self ! {X, Self} end,
    F9 = fun(X, Y) -> Self ! {X, Y, Self} end,

    true = F3 == erlang:binary_to_term(erlang:term_to_binary(F3)),

    Funs = [
        F4,
        F2,
        fun erlang:exit/1,
        F9,
        F3,
        F6,
        fun application:ensure_started/1,
        F5,
        F8,
        F1,
        fun erlang:apply/3,
        F7
    ],
    false = has_duplicates(Funs),
    true = ?MODULE:id(Funs) == ?MODULE:id(Funs),
    true = ?MODULE:id(Funs) =:= ?MODULE:id(Funs),
    Sorted = sort(Funs),
    Sorted = [
        F1,
        F2,
        F3,
        F4,
        F5,
        F6,
        F7,
        F8,
        F9,
        fun application:ensure_started/1,
        fun erlang:apply/3,
        fun erlang:exit/1
    ],
    true = Sorted < [fun erlang:whereis/1],
    true = Sorted > {fun erlang:whereis/1},
    0.

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

has_duplicates([]) ->
    false;
has_duplicates([H | T]) ->
    case lists:member(H, T) of
        true -> true;
        false -> has_duplicates(T)
    end.

increase_last_byte(Binary) ->
    Size = byte_size(Binary) - 1,
    <<Prefix:Size/binary, V:8>> = Binary,
    <<Prefix/binary, (V + 1):8>>.

id(X) -> X.
