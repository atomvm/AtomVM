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

-module(literal_test0).

-export([start/0, factorial/1, id/1, t/5]).

%% erlfmt-ignore
start() ->
    N0 = factorial(0),
    N1 = factorial(1),
    N2 = factorial(3),
    N3 = factorial(4),
    N4 = factorial(5),
    A = {N0 + N2 + N4, N1 + N3},
    B = [{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}, {f, 6},
         {g, 7}, {h, 8}, {i, 9}, {j, 10}, {k, 11}, {l, 12},
         {m, 13}, {n, 14}, {o, 15}, {p, 16}, {q, 17}, {r, 18}],
    C = {N0, N1, N2, N3, N4, A, B},
    E = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
    D = [E, a, A, b, N0, c, B, d, N1, e, C, f, N2, g, N3, h, N4,
         g, N0, h, N1, i, N2, j, N3, k, N4, l, N0, l, C, N0, m, B, N1, n, A, N2,
        o, N3, p, N4, p, N3, r, N2, s, N1, N0, E],
    t(id(D), id(A), id(C), id(B), id(D)).

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

id(I) when is_tuple(I) orelse is_list(I) ->
    I;
id(I) when is_integer(I) ->
    integer.

t(List1, {A, B}, {C, D, E, F, G, _, _}, [{_HK, HV} | _T], List2) ->
    A * C * D * E * F * G * (A + B) * HV + length(List1) * length(List2).
