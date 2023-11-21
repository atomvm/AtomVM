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

-module(patternmatchfunc).

-export([start/0, test_func/1]).

start() ->
    test_func([10, 20, 30, 3]) + test_func(false).

test_func([_ | T]) ->
    test_func(T);
test_func([]) ->
    0;
test_func(0) ->
    16;
test_func(1) ->
    17;
test_func(2) ->
    18;
test_func(3) ->
    19;
test_func(4) ->
    20;
test_func(5) ->
    21;
test_func(100) ->
    22;
test_func(nil) ->
    100;
test_func(true) ->
    101;
test_func(false) ->
    102;
test_func({_A}) ->
    test_func_a;
test_func({ok, _X}) ->
    ok;
test_func({error, X}) ->
    {error, X};
test_func({A, b, C}) ->
    {test_func_b_1, A, C};
test_func({_A, _B, c}) ->
    test_func_b_2;
test_func({_A, B, _C}) ->
    {test_func_b_3, B};
test_func({_A, _B, _C, _D}) ->
    test_func_c;
test_func(error) ->
    error.
