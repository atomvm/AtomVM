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

-module(test_bigint_eq).

-export([start/0, id/1, make_list/1, pow/2]).

%% erlfmt-ignore
start() ->
    N = factorial(id(0)) + 62,
    bool_to_n(make_list(N) == [pow(-2, 62),
                        {5, pow(-2, 63)},
                        [1, test],
                        [{}, []],
                        [5, {}]]) +
    bool_to_n(make_list(N) == [pow(-2, 63),
                        {5, pow(-2, 62)},
                        [1, test],
                        [{}, []],
                        [5, {}]]) * 2 +
    bool_to_n(make_list(N) == [pow(-2, 62),
                        {5, {pow(-2, 62)}},
                        [1, test],
                        [{}, []],
                        [5, {}]]) * 4.

make_list(N) ->
    [
        pow(-2, N - 1),
        {5, pow(-2, N)},
        [1, test],
        [{}, []],
        [5, {}]
    ].

pow(_N, 0) ->
    1;
pow(N, M) ->
    N * pow(N, M - 1).

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

id(N) ->
    N.

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
