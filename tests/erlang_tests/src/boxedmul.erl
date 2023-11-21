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

-module(boxedmul).

-export([start/0, pow/2, mulM1/1, neg/1, id/1]).

start() ->
    (mulM1(pow(-2, 31)) - 1) rem (mulM1(pow(-2, 27)) - 1).

pow(N, 0) when is_number(N) ->
    1;
pow(N, M) ->
    N * pow(N, M - 1).

mulM1(M) ->
    M * neg(1).

neg(N) ->
    -id(N).

id(I) when not is_tuple(I) ->
    I.
