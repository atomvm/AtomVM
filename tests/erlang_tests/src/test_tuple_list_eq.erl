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

-module(test_tuple_list_eq).

-export([start/0, id/1, make_tuple/1, factorial/1]).

start() ->
    N = factorial(id(0)) - 1,
    bool_to_n(
        make_tuple(id(N)) ==
            {[1, 1, test, 2], [1, 1, test, 2], [1, test, 2, 2], [1, test, {6}, 2], [
                1,
                test,
                {[{24}], test},
                2
            ]}
    ) +
        bool_to_n(
            make_tuple(id(N)) ==
                {[1, 1, test, 2], [1, 1, test, 2], [1, test, 2, 2], [1, test, {6}, 2], [
                    1,
                    test,
                    {[{24, 1}], test},
                    2
                ]}
        ) * 2 +
        bool_to_n(
            make_tuple(id(N)) ==
                {[1, 1, test, 2], [1, 1, test, 2], [1, test, 2, 2], [1, test, {6}, 2], [
                    1,
                    test,
                    {[{25}], test},
                    2
                ]}
        ) * 4.

make_tuple(N) ->
    {
        [factorial(N), 1, test, 2],
        [1, factorial(N + 1), test, 2],
        [1, test, 2, factorial(N + 2)],
        [1, test, {factorial(N + 3)}, 2],
        [1, test, {[{factorial(N + 4)}], test}, 2]
    }.

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

id(N) ->
    N.

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
