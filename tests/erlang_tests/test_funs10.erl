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

-module(test_funs10).

-export([start/0, f/4]).

start() ->
    A = val("1"),
    Op = op(mul),
    L1 = [
        [1, 2, 3, 4],
        [5, 6, 7, 8],
        [8, 9, 10, 11]
    ],
    L2 = [
        [20, 30, 40, 50],
        [60, 70, 80, 90],
        [100, 110, 120, 130]
    ],
    f(L1, L2, 0, fun(P, Q) -> f(P, Q, A, fun(X, Y) -> Op(X, Y) end) end).

f([H1 | T1], [H2 | T2], Acc, F) when is_function(F) ->
    R = F(H1, H2),
    f(T1, T2, R + Acc, F);
f([], [], Acc, _F) ->
    Acc.

val(A) when is_list(A) ->
    -list_to_integer(A).

op(mul) ->
    fun(A, B) -> A * B end;
op(add) ->
    fun(A, B) -> A + B end;
op(_X) ->
    fun(_A, _B) -> 0 end.
