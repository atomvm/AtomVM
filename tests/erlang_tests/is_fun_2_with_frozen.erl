%
% This file is part of AtomVM.
%
% Copyright 2020 Davide Bettio <davide@uninstall.it>
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

-module(is_fun_2_with_frozen).

-export([start/0, f/2, g/1, factorial/1, id/1]).

start() ->
    factorial(g(id(f(factorial(1), id(2))))).

f(A, B) ->
    C = factorial(A) + factorial(B),
    id(fun() -> id(C) end).

id(X) ->
    X.

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N - 1).

g(X) when is_function(X, 1) ->
    -1;
g(X) when is_function(X, 0) ->
    id(factorial(id(3))) - factorial(2);
g(X) when is_function(X) ->
    1;
g(_X) ->
    0.
