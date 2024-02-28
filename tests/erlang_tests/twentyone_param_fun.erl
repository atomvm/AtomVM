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

-module(twentyone_param_fun).
-export([start/0, sum_mul/3, id/1, g/18]).

start() ->
    {X, Fun} = ?MODULE:g(20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37),
    Ref = erlang:make_ref(),
    {A, Ref2} = Fun(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, Ref),
    {B, Ref} = Fun(
        1, 2, 3, 40, 50, 60, 70, 80, 90, 100, 110, 120, 13, 14, 15, 16, 17, 18, 19, X, Ref2
    ),
    1337 - (A + B).

g(C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, D, C11, C12, C13, C14, C15, C16, C17) when
    is_integer(C17)
->
    Fun =
        (fun(
            P1,
            P2,
            P3,
            P4,
            P5,
            P6,
            P7,
            P8,
            P9,
            P10,
            P11,
            P12,
            P13,
            P14,
            P15,
            P16,
            P17,
            P18,
            P19,
            P20,
            Ref
        ) when is_reference(Ref) ->
            Sum =
                ?MODULE:sum_mul(P1 - C1, P2, 1) +
                    ?MODULE:sum_mul(P3, P4 - C2, 2) +
                    ?MODULE:sum_mul(P5 - C3, P6, 3) +
                    ?MODULE:sum_mul(P5, P6 - C4, 4) +
                    ?MODULE:sum_mul(P7 - C5, P8, 5) +
                    ?MODULE:sum_mul(P9, P10 - C6, 6) +
                    ?MODULE:sum_mul(P11 - C7, P12, 7) +
                    ?MODULE:sum_mul(P13 - C9, P14 - C8, 8) +
                    ?MODULE:sum_mul(P15 - C10, P16 - C11, 9) +
                    ?MODULE:sum_mul(P17 - C13, P18 - C12, 10) +
                    ?MODULE:sum_mul(P19 - C14, P20 - C15, 11) + C16 - C17,
            {?MODULE:id(Sum), ?MODULE:id(Ref)}
        end),
    {D, Fun}.

sum_mul(A, B, C) ->
    (A + B) * C.

id(X) ->
    X.
