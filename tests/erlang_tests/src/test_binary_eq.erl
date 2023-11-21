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

-module(test_binary_eq).

-export([start/0, id/1, make_list/2, factorial/1]).

%% erlfmt-ignore
start() ->
    N = factorial(id(0)) - 1,
    bool_to_n(make_list(<<"test">>, N) == [{1,<<"es">>},
                                 {<<"es">>,<<"1">>},
                                 [0,<<"es">>],
                                 [<<"es">>,0],
                                 [0,0]]) +
    bool_to_n(make_list(<<"test">>, N) == [{1,<<"es">>},
                                 {<<"es">>,<<"1">>},
                                 [0,<<"ez">>],
                                 [<<"es">>,0],
                                 [0,0]]) * 2 +
    bool_to_n(make_list(<<"test">>, N) == [{1,<<"es">>},
                                 {<<"es">>,<<"1">>},
                                 [0,<<"es">>],
                                 [0,<<"es">>,0],
                                 [0,0]]) * 4.

make_list(Bin, N) ->
    B = binary:part(Bin, 1, 2),
    [
        {1, B},
        {B, integer_to_binary(factorial(N))},
        [N, B],
        [B, N],
        [N, N]
    ].

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

id(N) ->
    N.

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
