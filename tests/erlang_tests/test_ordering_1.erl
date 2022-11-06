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

-module(test_ordering_1).

-export([start/0, id/1, x/2]).

start() ->
    R = make_ref(),
    L1 =
        {[
            {[
                {R, [], "foo", 0, {<<"bar">>, [1, 2, 3]}, [[a], b], {{{{}}}}, [[[R], 1], 2], {
                    pow(-2, 63), <<"foo">>
                }}
            ]}
        ]},
    L2 =
        {[
            {[
                {
                    id(R),
                    id([]),
                    binary_to_list(id(<<"foo">>)),
                    id(pow(5, 0)) - 1,
                    {id(<<"bar">>), [pow(id(1), 0), id(pow(2, 1)), id(pow(3, 1))]},
                    id([[id(list_to_atom("a"))], b]),
                    {{id({{}})}},
                    [[[id(id(R))], pow(6, id(0))], -pow(id(-2), 1)],
                    {id(pow(-2, 63) + 1), list_to_binary(id("foo"))}
                }
            ]}
        ]},
    bool_to_n(L1 < L2).

pow(_N, 0) ->
    1;
pow(N, M) ->
    N * pow(N, M - 1).

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.

id(I) when is_binary(I) ->
    x(I, byte_size(I));
id(I) ->
    x(I, 5).

x(V, K) when K div 2 /= 0 ->
    x(V, K div 2);
x(V, 1) ->
    V.
