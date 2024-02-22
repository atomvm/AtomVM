%
% This file is part of AtomVM.
%
% Copyright 2024 Davide Bettio <davide@uninstall.it>
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

-module(twentyone_param_function).
-export([start/0, f/21, sum_mul/3, id/1, get_mf/0]).

start() ->
    Ref = erlang:make_ref(),
    {M, F} = ?MODULE:get_mf(),
    {A, Ref2} = f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, Ref),
    {B, Ref} = ?MODULE:f(
        1, 2, 3, 40, 50, 60, 70, 80, 90, 100, 110, 120, 13, 14, 15, 16, 17, 18, 19, 20, Ref2
    ),
    {C, Ref} = M:F(
        -1, 2, -3, 4, -5, 6, -7, 8, -9, 10, -11, 12, -13, 14, -15, 16, -17, 18, -19, 20, Ref
    ),
    A + B + C - 7417.

f(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, Ref) ->
    Sum =
        ?MODULE:sum_mul(P1, P2, 1) +
            ?MODULE:sum_mul(P3, P4, 2) +
            ?MODULE:sum_mul(P5, P6, 3) +
            ?MODULE:sum_mul(P5, P6, 4) +
            ?MODULE:sum_mul(P7, P8, 5) +
            ?MODULE:sum_mul(P9, P10, 6) +
            ?MODULE:sum_mul(P11, P12, 7) +
            ?MODULE:sum_mul(P13, P14, 8) +
            ?MODULE:sum_mul(P15, P16, 9) +
            ?MODULE:sum_mul(P17, P18, 10) +
            ?MODULE:sum_mul(P19, P20, 11),
    {?MODULE:id(Sum), ?MODULE:id(Ref)}.

sum_mul(A, B, C) ->
    (A + B) * C.

id(X) ->
    X.

get_mf() ->
    {?MODULE, f}.
