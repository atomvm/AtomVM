%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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

-module(test_apply).

-export([test/0, f/0, f/1, f/2, f/3, f/4, f/5, f/6]).

test() ->
    ok = test_apply_3(),
    ok = test_apply_2(),
    ok.

%% apply/2 and apply/3 are not nifs but implemented
%% in erlang module.

test_apply_3() ->
    0 = apply(?MODULE, f, []),
    1 = apply(?MODULE, f, [1]),
    3 = apply(?MODULE, f, [1, 2]),
    6 = apply(?MODULE, f, [1, 2, 3]),
    11 = apply(?MODULE, f, [1, 2, 3, 5]),
    19 = apply(?MODULE, f, [1, 2, 3, 5, 8]),
    32 = apply(?MODULE, f, [1, 2, 3, 5, 8, 13]),
    ok.

test_apply_2() ->
    0 = apply(fun ?MODULE:f/0, []),
    1 = apply(fun ?MODULE:f/1, [1]),
    3 = apply(fun ?MODULE:f/2, [1, 2]),
    6 = apply(fun ?MODULE:f/3, [1, 2, 3]),
    11 = apply(fun ?MODULE:f/4, [1, 2, 3, 5]),
    19 = apply(fun ?MODULE:f/5, [1, 2, 3, 5, 8]),
    32 = apply(fun ?MODULE:f/6, [1, 2, 3, 5, 8, 13]),
    ok.

f() -> 0.
f(A) -> A.
f(A, B) -> A + B.
f(A, B, C) -> A + B + C.
f(A, B, C, D) -> A + B + C + D.
f(A, B, C, D, E) -> A + B + C + D + E.
f(A, B, C, D, E, F) -> A + B + C + D + E + F.
