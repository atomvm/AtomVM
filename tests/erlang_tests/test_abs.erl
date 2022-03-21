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

-module(test_abs).

-export([start/0, i/2, f/2, g/2]).

start() ->
    i([1, 2, 3], a) + i([1, 2], b) + i([], a).

i(L, a) ->
    abs(f(L, 0));
i(L, b) ->
    abs(g(L, 0)).

f([], Acc) ->
    -Acc;
f([_H | T], Acc) ->
    f(T, Acc + 1).

g([], Acc) ->
    Acc;
g([_H | T], Acc) ->
    f(T, Acc + 1).
