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

-module(float_is_number).

-export([start/0, pow/2, test/1, id/1]).

start() ->
    Res = ?MODULE:id((pow(-2, 63) + id(10.0)) * id(-1.0)),
    true = ?MODULE:id(is_number(Res)),
    test(Res).

id(I) when is_float(I) ->
    I;
id(I) when is_atom(I) ->
    I.

pow(_N, 0) ->
    1;
pow(N, M) ->
    N * pow(N, M - 1).

test(Val) when is_number(Val) ->
    32;
test(Val) when is_integer(Val) ->
    16;
test(_Val) ->
    8.
