%
% This file is part of AtomVM.
%
% Copyright 2024 Davide Bettio <davide@uninstall.it>
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

-module(test_sets).

-export([test/0]).

-include("etest.hrl").

test() ->
    ok = test_from_list(),
    ok = test_is_set(),
    ok = test_size(),
    ok = test_is_equal(),
    ok = test_is_element(),
    ok = test_add_element(),
    ok = test_del_element(),
    ok = test_union(),
    ok = test_intersection(),
    ok = test_subtract(),
    ok = test_is_disjoint(),
    ok = test_is_subset(),
    ok = test_filter(),
    ok = test_map(),
    ok = test_filtermap(),
    ok.

test_from_list() ->
    ?ASSERT_MATCH(sets:from_list([ok, <<"hello">>], [{version, 2}]), #{ok => [], <<"hello">> => []}),
    ok.

test_is_set() ->
    ?ASSERT_MATCH(sets:is_set(sets:from_list([ok, <<"hello">>])), true),
    ?ASSERT_MATCH(sets:is_set({}), false),
    ok.

test_size() ->
    ?ASSERT_MATCH(sets:size(sets:from_list([ok, <<"hello">>])), 2),
    ?ASSERT_MATCH(sets:size(sets:from_list([])), 0),
    ok.

test_is_equal() ->
    ?ASSERT_MATCH(sets:is_equal(sets:from_list([1, 2]), sets:from_list([1, 2])), true),
    ?ASSERT_MATCH(sets:is_equal(sets:from_list([1, 2]), sets:from_list([1, 3])), false),
    ?ASSERT_MATCH(sets:is_equal(sets:from_list([1, 2]), sets:from_list([1, 2.0])), false),
    ?ASSERT_MATCH(sets:is_equal(sets:from_list([1, 2]), #{1 => true, 2 => true}), true),
    ?ASSERT_MATCH(sets:is_equal(sets:from_list([1, 2]), #{1 => true, 3 => true}), false),
    ok.

test_is_element() ->
    ?ASSERT_MATCH(sets:is_element(1, sets:from_list([1, 2])), true),
    ?ASSERT_MATCH(sets:is_element(1, sets:from_list([1.0, 3])), false),
    ?ASSERT_MATCH(sets:is_element(5, sets:from_list([1, 2])), false),
    ?ASSERT_MATCH(sets:is_element(7, sets:from_list([])), false),
    ok.

test_add_element() ->
    ?ASSERT_MATCH(
        sets:is_equal(
            sets:add_element(10, sets:from_list([20, 30])),
            sets:from_list([10, 20, 30])
        ),
        true
    ),
    ok.

test_del_element() ->
    ?ASSERT_MATCH(
        sets:is_equal(
            sets:del_element(30, sets:from_list([20, 30])),
            sets:from_list([20])
        ),
        true
    ),
    ?ASSERT_MATCH(
        sets:is_equal(
            sets:del_element(-1, sets:from_list([1, 2])),
            sets:from_list([1, 2])
        ),
        true
    ),
    ?ASSERT_MATCH(
        sets:is_equal(
            sets:del_element(1, sets:from_list([])),
            sets:from_list([])
        ),
        true
    ),
    ok.

test_union() ->
    ?ASSERT_MATCH(
        sets:is_equal(
            sets:union(sets:from_list([1, 2, 3]), sets:from_list([4, 5])),
            sets:from_list([1, 2, 3, 4, 5])
        ),
        true
    ),
    ok.

test_intersection() ->
    ?ASSERT_MATCH(
        sets:is_equal(
            sets:intersection(sets:from_list([1, 2, 3]), sets:from_list([0, 2, 3, 7])),
            sets:from_list([2, 3])
        ),
        true
    ),
    ok.

test_subtract() ->
    ?ASSERT_MATCH(
        sets:is_equal(
            sets:subtract(sets:from_list([1, 2, 3, 4]), sets:from_list([2, 3])),
            sets:from_list([1, 4])
        ),
        true
    ),
    ok.

test_is_disjoint() ->
    ?ASSERT_MATCH(sets:is_disjoint(sets:from_list([1, 2, 3]), sets:from_list([4, 5, 6])), true),
    ?ASSERT_MATCH(sets:is_disjoint(sets:from_list([1, 2, 3]), sets:from_list([3.0, 4])), true),
    ?ASSERT_MATCH(sets:is_disjoint(sets:from_list([1, 2]), sets:from_list([2, 3])), false),
    ok.

test_is_subset() ->
    ?ASSERT_MATCH(sets:is_subset(sets:from_list([2, 3]), sets:from_list([1, 2, 3, 4])), true),
    ?ASSERT_MATCH(sets:is_subset(sets:from_list([2.0, 3]), sets:from_list([1, 2, 3, 4])), false),
    ?ASSERT_MATCH(sets:is_subset(sets:from_list([-1]), sets:from_list([1, 2, 3, 4])), false),
    ok.

test_filter() ->
    ?ASSERT_MATCH(
        sets:is_equal(
            sets:filter(fun(X) -> X < 3 end, sets:from_list([1, 2, 3, 4, 5, 6])),
            sets:from_list([1, 2])
        ),
        true
    ),
    ok.

test_map() ->
    ?ASSERT_MATCH(
        sets:is_equal(
            sets:map(fun(X) -> X * 2 end, sets:from_list([1, 3, 5])),
            sets:from_list([2, 6, 10])
        ),
        true
    ),
    ok.

test_filtermap() ->
    ?ASSERT_MATCH(
        sets:is_equal(
            sets:filtermap(
                fun(X) ->
                    case X rem 2 of
                        0 -> {true, X div 2};
                        _ -> false
                    end
                end,
                sets:from_list([1, 2, 3, 4, 5])
            ),
            sets:from_list([1, 2])
        ),
        true
    ),
    ok.
