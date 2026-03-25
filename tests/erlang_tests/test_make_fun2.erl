%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

%% @doc Test make_fun2 (opcode 103) and make_fun3 (opcode 171).
%% OTP 26 with no_make_fun3 emits make_fun2.
%% OTP 26 default and OTP 27+ emit make_fun3 instead.
-module(test_make_fun2).

-export([start/0]).

-if(?OTP_RELEASE =:= 26).
-compile([no_make_fun3]).
-endif.

start() ->
    ok = test_closure_one_var(),
    ok = test_closure_two_vars(),
    ok = test_closure_no_vars(),
    ok = test_closure_nested(),
    0.

%% Closure capturing one variable.
test_closure_one_var() ->
    F = make_adder(id(10)),
    15 = F(id(5)),
    20 = F(id(10)),
    ok.

%% Closure capturing two variables.
test_closure_two_vars() ->
    F = make_range_checker(id(1), id(10)),
    true = F(id(5)),
    false = F(id(11)),
    ok.

%% Closure capturing no variables (just a fun reference).
test_closure_no_vars() ->
    F = make_doubler(),
    14 = F(id(7)),
    ok.

%% Nested closures.
test_closure_nested() ->
    F = make_multiplier(id(3)),
    G = F(id(4)),
    12 = G(id(1)),
    ok.

make_adder(N) ->
    fun(X) -> X + N end.

make_range_checker(Lo, Hi) ->
    fun(X) -> X >= Lo andalso X =< Hi end.

make_doubler() ->
    fun(X) -> X * 2 end.

make_multiplier(A) ->
    fun(B) ->
        fun(C) -> A * B * C end
    end.

id(X) -> X.
