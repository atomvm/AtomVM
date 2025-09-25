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

-module(test_lists_member).

-export([start/0, id/1]).

-define(ID(X), ?MODULE:id(X)).

id(X) -> X.

start() ->
    true = test_member_with_existing_element(),
    true = test_member_with_existing_element_on_heterogenous_list(),
    false = test_member_with_non_existing_element(),
    false = test_member_in_empty_list(),
    ok = test_member_with_invalid_input(),
    0.

test_member_with_existing_element() ->
    Element = ?ID(5),
    List = ?ID([1, 2, 3, 4, 5, 6, 7, 8, 9]),
    lists:member(Element, List).

test_member_with_existing_element_on_heterogenous_list() ->
    Element = ?ID(key),
    List = ?ID([1, "a", 3, "b", 5, 6, "cd", 8, key]),
    lists:member(Element, List).

test_member_with_non_existing_element() ->
    Element = ?ID(10),
    List = ?ID([1, 2, 3, 4, 5, 6, 7, 8, 9]),
    lists:member(Element, List).

test_member_in_empty_list() ->
    Element = ?ID(element),
    List = ?ID([]),
    lists:member(Element, List).

test_member_with_invalid_input() ->
    ok =
        try
            lists:member(?ID(element), ?ID(not_a_list)),
            error
        catch
            error:badarg -> ok
        end,
    ok =
        try
            lists:member(?ID(element), ?ID([a, b | foo])),
            error
        catch
            error:badarg -> ok
        end.
