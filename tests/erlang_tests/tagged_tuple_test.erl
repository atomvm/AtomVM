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

-module(tagged_tuple_test).

-export([start/0, test_func/1, call_test_func/1, some_value/1]).

start() ->
    call_test_func({test, some_value(1)}).

call_test_func({test, V}) ->
    test_func({c, V}).

some_value(A) ->
    A * 2 + 1.

test_func({a, A}) ->
    A;
test_func({b, B}) ->
    B * 2;
test_func({E_A, E}) when is_atom(E_A) ->
    E * 16;
test_func({f, F, F_B}) ->
    F - F_B;
test_func(G) when is_tuple(G) ->
    0;
test_func({A, B}) when is_integer(A) and is_integer(B) ->
    A - B.
