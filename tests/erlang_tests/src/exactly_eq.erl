%
% This file is part of AtomVM.
%
% Copyright 2023 Davide Bettio <davide@uninstall.it>
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

-module(exactly_eq).

-export([start/0, fact/3]).

start() ->
    A = fact(4, 1, 1),
    B = fact(4, 1.0, 1.0),
    test(not (A =:= B)) +
        test(A =/= B) * 2 +
        test(A == B) * 4.

test(true) ->
    1;
test(false) ->
    0.

fact(N, _D, Acc) when N < 1 ->
    Acc;
fact(N, D, Acc) ->
    fact(N - D, D, Acc * N).
