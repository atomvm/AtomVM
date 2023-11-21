%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_lists_reverse).

-export([start/0]).

start() ->
    ok = test_lists_reverse_1(),
    ok = test_lists_reverse_2(),
    0.

test_lists_reverse_1() ->
    [] = lists:reverse([]),
    [a, b, c] = lists:reverse([c, b, a]),
    ok =
        try
            lists:reverse([a, b | improper]),
            fail
        catch
            error:badarg -> ok
        end,
    ok =
        try
            lists:reverse(not_a_list),
            fail
        catch
            % AtomVM
            error:badarg -> ok;
            % BEAM
            error:function_clause -> ok
        end,
    ok.

test_lists_reverse_2() ->
    [] = lists:reverse([], []),
    [a, b, c, d, e, f] = lists:reverse([c, b, a], [d, e, f]),
    ok =
        try
            lists:reverse([a, b | improper], [a]),
            fail
        catch
            % AtomVM
            error:badarg -> ok;
            % BEAM
            T:V -> {T, V}
        end,
    [b, a, c, d | improper] = lists:reverse([a, b], [c, d | improper]),
    ok =
        try
            lists:reverse(not_a_list, []),
            fail
        catch
            error:badarg -> ok
        end,
    not_a_list = lists:reverse([], not_a_list),
    [c, b, a | not_a_list] = lists:reverse([a, b, c], not_a_list),
    ok.
