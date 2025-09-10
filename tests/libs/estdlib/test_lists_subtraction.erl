%
% This file is part of AtomVM.
%
% Copyright 2024 Tomasz Sobkiewicz <tomasz.sobkiewicz@swmansion.com>
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
-module(test_lists_subtraction).

-export([test/0, sub/2, start/0]).
start() ->
    test().

test() ->
    Tests = [
        {[1, 2, 3], [1], [2, 3]},
        {[1, 2, 3], [3], [1, 2]},
        {[1, 2, 3], [5], [1, 2, 3]},
        {[1, 2, 2, 3], [2], [1, 2, 3]},
        {[1, 2, 3, 4], [2, 2], [1, 3, 4]},
        {["a", 1, "b", 2], ["a", 2], [1, "b"]},
        {[1, 2, 3], [1, 2, 3, 4, 5], []},
        {[1, 2, 3], [], [1, 2, 3]},
        {[], [1, 2, 3], []},
        {[a, b, b, c], [c, b, a], [b]},
        {[1, 2, 3, 4], [4, 3, 2, 1], []},
        {[1, 2, 3, 4], [1, 2, 3, 4], []},
        {[1], [1.0], [1]},
        {[1.0], [1.0], []},
        {[1, 2, 3, 4, 5, 6, 7, 8, 9], ?MODULE:sub([1, 2, 3, 4], [1, 2, 5, 6, 7, 8, 9]), [
            1, 2, 5, 6, 7, 8, 9
        ]}
    ],
    ok = expect_failure(fun() -> ?MODULE:sub([a, b | c], [a]) end, badarg),
    ok = expect_failure(fun() -> ?MODULE:sub([], [a, b | c]) end, badarg),
    ok = expect_failure(fun() -> ?MODULE:sub([a], [a, b | c]) end, badarg),
    run_tests(Tests).

run_tests(Tests) ->
    [Expected = A -- B || {A, B, Expected} <- Tests],
    ok.

expect_failure(Fun, Error) ->
    try
        Fun(),
        fail
    catch
        error:Error ->
            ok
    end.

sub(A, B) ->
    A -- B.
