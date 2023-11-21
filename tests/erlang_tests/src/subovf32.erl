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

-module(subovf32).

-export([start/0, pow/2, neg_id/1, id/1]).

start() ->
    Res = (pow(-2, 59)),
    AlwaysBoxed1 = neg_id(Res - 1),
    L1 = to_list(AlwaysBoxed1, []),
    I1 = make_int(L1, 1, 0),
    AlwaysBoxed2 = -1 - AlwaysBoxed1,
    L2 = to_list(AlwaysBoxed2, []),
    I2 = make_int(L2, 1, 0),
    I1 + I2.

pow(_N, 0) ->
    1;
pow(N, M) ->
    N * pow(N, M - 1).

to_list(0, Acc) ->
    Acc;
to_list(N, Acc) ->
    to_list(N div 10, [N rem 10 | Acc]).

make_int([], _N, Acc) ->
    Acc;
make_int([H | T], N, Acc) ->
    make_int(T, N + 2, H * N + Acc).

neg_id(0) ->
    test;
neg_id(N) when is_integer(N) ->
    -id(N).

id(N) when is_integer(N) ->
    N.
