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

-module(test_tl).

-export([start/0, f/2, g/2, h/1]).

start() ->
    h(g(b, f(3, 1))).

f(0, _V) ->
    [];
f(1, V) ->
    [V];
f(2, V) ->
    [V, V + 1];
f(3, V) ->
    [V, V + 1, V + 2].

g(a, _L) ->
    0;
g(b, L) ->
    tl(L).

h([A, B]) ->
    A + B;
h(_) ->
    20.
