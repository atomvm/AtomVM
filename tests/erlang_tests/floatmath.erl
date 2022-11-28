%
% This file is part of AtomVM.
%
% Copyright 2022 Fred Dushin <fred@dushin.net>
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

-module(floatmath).

-export([start/0, id/1, to_int/1]).

-define(PI_OVER_2, 1.570796325).
-define(THREE_PI_OVER_2, 4.712388975).
-define(PI_OVER_4, 0.7853981625).
-define(THREE_QUARTERS_PI, 2.3561944875).

start() ->
    ok = test_sin(),
    ok = test_cos(),
    ok = test_tan(),
    ok = test_asin(),
    ok = test_acos(),
    ok = test_atan(),

    ok = test_sinh(),
    ok = test_cosh(),
    ok = test_tanh(),
    ok = test_asinh(),
    ok = test_acosh(),
    ok = test_atanh(),

    ok = test_floor(),
    ok = test_ceil(),
    ok = test_pow(),
    ok = test_exp(),
    ok = test_sqrt(),
    ok = test_log(),
    ok = test_log10(),
    ok = test_log2(),
    0.

test_sin() ->
    0 = to_int(math:sin(id(0))),
    0 = to_int(math:sin(id(0.0))),
    1 = to_int(math:sin(id(?PI_OVER_2))),
    0 = to_int(math:sin(id(math:pi()))),
    -1 = to_int(math:sin(id(?THREE_PI_OVER_2))),
    ok.

test_cos() ->
    1 = to_int(math:cos(id(0))),
    1 = to_int(math:cos(id(0.0))),
    0 = to_int(math:cos(id(?PI_OVER_2))),
    -1 = to_int(math:cos(id(math:pi()))),
    0 = to_int(math:cos(id(?THREE_PI_OVER_2))),
    ok.

test_tan() ->
    1 = to_int(math:tan(id(?PI_OVER_4))),
    -1 = to_int(math:tan(id(?THREE_QUARTERS_PI))),
    ok.

test_asin() ->
    0 = to_int(math:asin(id(0))),
    0 = to_int(math:asin(id(0.0))),
    badarith = catch_error(fun() -> to_int(math:asin(id(2.0))) end),
    ok.

test_acos() ->
    2 = to_int(math:acos(id(0))),
    2 = to_int(math:acos(id(0.0))),
    badarith = catch_error(fun() -> to_int(math:acos(id(2.0))) end),
    ok.

test_atan() ->
    0 = to_int(math:atan(id(0.0))),
    1 = to_int(math:atan(id(2.0))),
    -1 = to_int(math:atan(id(-2.0))),
    ok.

test_sinh() ->
    0 = to_int(math:sinh(id(0))),
    0 = to_int(math:sinh(id(0.0))),
    1 = to_int(math:sinh(id(1.0))),
    4 = to_int(math:sinh(id(2.0))),
    -1 = to_int(math:sinh(id(-1.0))),
    -4 = to_int(math:sinh(id(-2.0))),
    ok.

test_cosh() ->
    1 = to_int(math:cosh(id(0))),
    1 = to_int(math:cosh(id(0.0))),
    1 = to_int(math:cosh(id(0.5))),
    1 = to_int(math:cosh(id(-0.5))),
    ok.

test_tanh() ->
    0 = to_int(math:tanh(id(0))),
    0 = to_int(math:tanh(id(0.0))),
    1 = to_int(math:tanh(id(1.0))),
    -1 = to_int(math:tanh(id(-1.0))),
    ok.

test_asinh() ->
    0 = to_int(math:asinh(id(0))),
    0 = to_int(math:asinh(id(0.0))),
    1 = to_int(math:asinh(id(1.0))),
    -1 = to_int(math:asinh(id(-1.0))),
    ok.

test_acosh() ->
    0 = to_int(math:acosh(id(1))),
    0 = to_int(math:acosh(id(1.0))),
    badarith = catch_error(fun() -> to_int(math:acosh(id(0.0))) end),
    ok.

test_atanh() ->
    0 = to_int(math:atanh(id(0))),
    0 = to_int(math:atanh(id(0.0))),
    1 = to_int(math:atanh(id(0.5))),
    -1 = to_int(math:atanh(id(-0.5))),
    badarith = catch_error(fun() -> to_int(math:atanh(id(2.0))) end),
    badarith = catch_error(fun() -> to_int(math:atanh(id(-2.0))) end),
    ok.

test_floor() ->
    0 = to_int(math:floor(id(0))),
    0 = to_int(math:floor(id(0.0))),
    0 = to_int(math:floor(id(0.1))),
    -1 = to_int(math:floor(id(-0.1))),
    1 = to_int(math:floor(id(1.1))),
    -2 = to_int(math:floor(id(-1.1))),
    ok.

test_ceil() ->
    0 = to_int(math:ceil(id(0))),
    0 = to_int(math:ceil(id(0.0))),
    1 = to_int(math:ceil(id(0.1))),
    0 = to_int(math:ceil(id(-0.1))),
    2 = to_int(math:ceil(id(1.1))),
    -1 = to_int(math:ceil(id(-1.1))),
    ok.

test_pow() ->
    0 = to_int(math:pow(id(0), id(4))),
    16 = to_int(math:pow(id(2), id(4))),
    16 = to_int(math:pow(id(2.0), id(4.0))),
    1 = to_int(math:pow(id(2.0), id(-1.0))),
    2 = to_int(math:pow(id(4.0), id(0.5))),
    ok.

test_exp() ->
    1 = to_int(math:exp(id(0))),
    1 = to_int(math:exp(id(0.0))),
    7 = to_int(math:exp(id(2.0))),
    ok.

test_sqrt() ->
    0 = to_int(math:sqrt(id(0))),
    0 = to_int(math:sqrt(id(0.0))),
    1 = to_int(math:sqrt(id(2.0))),
    2 = to_int(math:sqrt(id(4.0))),
    2 = to_int(math:sqrt(id(4.0001))),
    badarith = catch_error(fun() -> to_int(math:sqrt(id(-1.0))) end),
    ok.

test_log() ->
    badarith = catch_error(fun() -> to_int(math:log(id(0))) end),
    badarith = catch_error(fun() -> to_int(math:log(id(0.0))) end),
    0 = to_int(math:log(id(1.0))),
    ok.

test_log10() ->
    badarith = catch_error(fun() -> to_int(math:log10(id(0))) end),
    badarith = catch_error(fun() -> to_int(math:log10(id(0.0))) end),
    0 = to_int(math:log10(id(1.0))),
    ok.

test_log2() ->
    badarith = catch_error(fun() -> to_int(math:log2(id(0))) end),
    badarith = catch_error(fun() -> to_int(math:log2(id(0.0))) end),
    0 = to_int(math:log2(id(1.0))),
    ok.

to_int(A) ->
    round(A).

id(I) ->
    I.

catch_error(F) ->
    try
        F(),
        fail
    catch
        error:E ->
            E
    end.
