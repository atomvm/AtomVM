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

-module(test_tuple_is_not_map).

-export([start/0, f/1, g/1, fact/1, id/1]).

start() ->
    id(g(id(fact(id(1))))).

fact(0) ->
    1;
fact(N) ->
    N * fact(N - 1).

id(X) when is_integer(X) ->
    X.

f(0) ->
    [];
f(1) ->
    {1};
f(_) ->
    <<"test">>.

g(X) when is_map(X) ->
    8;
g(_X) ->
    16.
