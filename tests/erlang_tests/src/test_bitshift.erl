%
% This file is part of AtomVM.
%
% Copyright 2021 Fred Dushin <fred@dushin.net>
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

-module(test_bitshift).

-export([start/0, shift_left/2, shift_right/2]).

start() ->
    test_shift(64 - 3),
    0.

test_shift(M) ->
    test_shift(0, M, 1).

test_shift(N, N, _E) ->
    ok;
test_shift(N, M, E) ->
    verify_shift(E, 16#01, N),
    test_shift(N + 1, M, E * 2).

verify_shift(E, A, B) ->
    E = shift_left(A, B),
    A = shift_right(E, B).

shift_left(A, B) ->
    A bsl B.

shift_right(A, B) ->
    A bsr B.
