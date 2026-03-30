%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

%% @doc Test y-register initialization opcodes.
%% OTP 26 with no_init_yregs emits allocate_zero (opcode 14).
%% OTP 26 default and OTP 27+ emit allocate + init_yregs (opcode 172) instead.
%% This module uses no_init_yregs on OTP 26 to exercise allocate_zero;
%% on OTP 27+ it exercises init_yregs with default compilation.
-module(test_allocate_zero).

-export([start/0]).

-if(?OTP_RELEASE =:= 26).
-compile([no_init_yregs]).
-endif.

start() ->
    ok = test_basic(),
    ok = test_conditional(),
    0.

%% Y register must survive across a call.
test_basic() ->
    A = id(10),
    B = add1(A),
    {10, 11} = {A, B},
    ok.

%% Conditional paths leave some y registers uninitialized unless zeroed
%% (allocate_zero) or explicitly initialized (init_yregs).
test_conditional() ->
    {0, 1, 2, 3} = foo(id(0)),
    {5, 6, 6, 7} = foo(id(5)),
    ok.

foo(A) ->
    B = add1(A),
    C =
        case B of
            1 -> add1(B);
            _ -> B
        end,
    D = add1(C),
    {A, B, C, D}.

add1(X) -> X + 1.

id(X) -> X.
