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

-module(test_funs9).

-export([start/0, plus5/1, f/2]).

start() ->
    C = {"10", "20", "30"},
    plus5(fun() -> g(C) end) +
        plus5(fun() -> 0 end) * 100.

plus5(F) ->
    F() + 5.

f(A, B) ->
    list_to_integer(A ++ B).

g({A, B, C}) ->
    f(A, B) + f(B, C).
