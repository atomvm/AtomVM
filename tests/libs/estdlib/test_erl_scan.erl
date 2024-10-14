%
% This file is part of AtomVM.
%
% Copyright 2024 Jakub Gonet <jakub.gonet@swmansion.com>
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
-module(test_erl_scan).

-export([test/0]).

test() ->
    {Fn, ExpectedFn} =
        {"fun(X) -> X + 1 end.", [
            {'fun', 1},
            {'(', 1},
            {var, 1, 'X'},
            {')', 1},
            {'->', 1},
            {var, 1, 'X'},
            {'+', 1},
            {integer, 1, 1},
            {'end', 1},
            {dot, 1}
        ]},

    {Comment, ExpectedComment} = {"% A comment", []},
    {Assignment, ExpectedAssignment} =
        {"A = 5,\nB = 10.", [
            {var, 1, 'A'},
            {'=', 1},
            {integer, 1, 5},
            {',', 1},
            {var, 2, 'B'},
            {'=', 2},
            {integer, 2, 10},
            {dot, 2}
        ]},
    {Maybe, ExpectedMaybe} = {
        "maybe\n"
        "  {ok, A} ?= a(),\n"
        "  true = A >= 0,\n"
        "  A\n"
        "end",
        [
            {'maybe', 1},
            {'{', 2},
            {atom, 2, ok},
            {',', 2},
            {var, 2, 'A'},
            {'}', 2},
            {'?=', 2},
            {atom, 2, a},
            {'(', 2},
            {')', 2},
            {',', 2},
            {atom, 3, true},
            {'=', 3},
            {var, 3, 'A'},
            {'>=', 3},
            {integer, 3, 0},
            {',', 3},
            {var, 4, 'A'},
            {'end', 5}
        ]
    },

    {ok, ExpectedFn, _} = erl_scan:string(Fn),
    {ok, ExpectedComment, _} = erl_scan:string(Comment),
    {ok, ExpectedAssignment, _} = erl_scan:string(Assignment),
    {ok, ExpectedMaybe, _} = erl_scan:string(Maybe),
    ok.
