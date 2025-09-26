%
% This file is part of AtomVM.
%
% Copyright 2025 Software Mansion
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

-module(test_lists_keyfind).

-export([start/0, id/1]).

-define(ID(X), ?MODULE:id(X)).

id(X) -> X.

start() ->
    ok = test_keyfind_with_existing_key(),
    ok = test_keyfind_with_non_existing_key(),
    ok = test_keyfind_with_empty_list(),
    ok = test_keyfind_with_existing_key_on_different_position(),
    ok = test_keyfind_with_existing_key_and_different_length_of_tuples(),
    ok = test_keyfind_with_invalid_input(),
    0.

test_keyfind_with_existing_key() ->
    Key = ?ID(bob),
    Tuple1 = ?ID({1, alice}),
    Tuple2 = ?ID({2, bob}),
    Tuple3 = ?ID({3, carol}),
    List = ?ID([Tuple1, Tuple2, Tuple3]),
    {2, bob} = lists:keyfind(Key, 2, List),
    ok.

test_keyfind_with_existing_key_and_different_length_of_tuples() ->
    Key = ?ID(is),
    Tuple1 = ?ID({1}),
    Tuple2 = ?ID({2, bob}),
    Tuple3 = ?ID({3, carol, singing, tree}),
    Tuple4 = ?ID({here, it, is}),
    List = ?ID([Tuple1, Tuple2, Tuple3, Tuple4]),
    {here, it, is} = lists:keyfind(Key, 3, List),
    ok.

test_keyfind_with_non_existing_key() ->
    Key = ?ID(4),
    Tuple1 = ?ID({1, alice}),
    Tuple2 = ?ID({2, bob}),
    Tuple3 = ?ID({3, carol}),
    List = ?ID([Tuple1, Tuple2, Tuple3]),
    false = lists:keyfind(Key, 1, List),
    ok.

test_keyfind_with_empty_list() ->
    Key = ?ID(3),
    List = ?ID([]),
    false = lists:keyfind(Key, 1, List),
    ok.

test_keyfind_with_existing_key_on_different_position() ->
    Key = ?ID(1),
    Tuple1 = ?ID({1, alice}),
    Tuple2 = ?ID({4, bob}),
    Tuple3 = ?ID({3, carol}),
    List = ?ID([Tuple1, Tuple2, Tuple3]),
    false = lists:keyfind(Key, 2, List),
    ok.

test_keyfind_with_invalid_input() ->
    ok =
        try
            lists:keyfind(key, 2, not_a_list),
            error
        catch
            error:badarg -> ok
        end,
    ok =
        try
            lists:keyfind(key, 2, [{a, 1}, {b, 2} | foo]),
            error
        catch
            error:badarg -> ok
        end.
