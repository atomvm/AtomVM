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

-module(literal_test1).

-export([start/0, id/1, f/2]).

start() ->
    length(id(f(id([]), 1))) +
        length(id(f(id([]), 2))) +
        length(id(f(id([]), 3))) +
        length(id(f(id([]), 4))) +
        length(id(f(id([]), 5))) +
        length(id(f(id([]), 10))) +
        length(id(f(id([]), 127))) +
        length(id(f(id([]), 128))).

f(L, 0) ->
    L;
f(L, N) ->
    Next = N - 1,
    T1 = {a, Next, b, N, c, N, d, N},
    T2 = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17},
    L1 = [T1, N, T2, Next, T1, T2, Next],
    T3 = {T1, L1, test, T2, a, N, b, Next, c, N, d, N, e, N, f, N, g, N, h, N, i, N},
    f([a, T1, b, T2, c, T3 | L], Next).

id(I) when is_tuple(I) orelse is_list(I) ->
    I;
id(I) when is_integer(I) ->
    integer.
