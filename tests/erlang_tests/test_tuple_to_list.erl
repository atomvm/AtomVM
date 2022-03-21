%
% This file is part of AtomVM.
%
% Copyright 2018 Davide Bettio <davide@uninstall.it>
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

-module(test_tuple_to_list).

-export([start/0, f/2, g/1]).

start() ->
    g(tuple_to_list(f({1, 2, 3, 4}, 10))).

f({A, B, C, D}, X) ->
    {A * X, B * X, C * X, D * X}.

g(L) ->
    g(L, 1, 0).

g([], _I, Acc) ->
    Acc;
g([H | T], I, Acc) ->
    g(T, I + 1, H * I + Acc).
