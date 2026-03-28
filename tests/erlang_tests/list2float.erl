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

-module(list2float).

-export([start/0, id/1, list2float/1, float_cmp/2, expect_badarg/1]).

start() ->
    F1 = list2float(?MODULE:id("1.5e+03")),
    F2 = list2float(?MODULE:id("1.5")),
    F3 = list2float(?MODULE:id("2.0")),
    F4 = list2float(?MODULE:id("-1.0")),
    F5 = list2float(?MODULE:id("0.1")),
    F6 = list2float(?MODULE:id("1.0e3")),
    F7 = list2float(?MODULE:id("1.0e-3")),
    F8 = list2float(?MODULE:id("0.0")),
    R0 =
        float_cmp(F1, ?MODULE:id(1500.0)) +
            float_cmp(F2, ?MODULE:id(1.5)) * 2 +
            float_cmp(F3, ?MODULE:id(2.0)) * 4 +
            float_cmp(F4, ?MODULE:id(-1.0)) * 8 +
            float_cmp(F5, ?MODULE:id(0.1)) * 16 +
            float_cmp(F6, ?MODULE:id(1000.0)) * 32 +
            float_cmp(F7, ?MODULE:id(0.001)) * 64 +
            float_cmp(F8, ?MODULE:id(0.0)) * 128 +
            expect_badarg(?MODULE:id({})) * 256,

    V1 = float_cmp(list2float(?MODULE:id("+1.0")), ?MODULE:id(1.0)),
    V2 = float_cmp(list2float(?MODULE:id("+000000000.07")), ?MODULE:id(0.07)),
    V3 = float_cmp(list2float(?MODULE:id("1.0E3")), ?MODULE:id(1000.0)),
    V4 = float_cmp(list2float(?MODULE:id("+1.0e+3")), ?MODULE:id(1000.0)),
    V5 = float_cmp(list2float(?MODULE:id("1.0e-999")), ?MODULE:id(0.0)),
    V6 = float_cmp(list2float(?MODULE:id("0.0e999")), ?MODULE:id(0.0)),
    V7 = float_cmp(list2float(?MODULE:id("-0.0")), ?MODULE:id(0.0)),
    V8 = float_cmp(list2float(?MODULE:id("1.0e-320")), ?MODULE:id(0.0)),
    V9 = float_cmp(list2float(?MODULE:id("5.0e-324")), ?MODULE:id(0.0)),
    V10 = float_cmp(list2float(?MODULE:id("2.0e-308")), ?MODULE:id(0.0)),

    B1 = expect_badarg(?MODULE:id("1.0e999")),
    B2 = expect_badarg(?MODULE:id("inf")),
    B3 = expect_badarg(?MODULE:id("nan")),
    B4 = expect_badarg(?MODULE:id("1e5")),
    B5 = expect_badarg(?MODULE:id(".5")),
    B6 = expect_badarg(?MODULE:id("1.")),
    B7 = expect_badarg(?MODULE:id(" 1.0")),
    B8 = expect_badarg(?MODULE:id("1.0 ")),
    B9 = expect_badarg(?MODULE:id("0x1.0p1")),
    B10 = expect_badarg(?MODULE:id("1,0")),
    B11 = expect_badarg(?MODULE:id("++1.0")),
    B12 = expect_badarg(?MODULE:id("1..0")),
    B13 = expect_badarg(?MODULE:id("1.0ee3")),
    B14 = expect_badarg(?MODULE:id("1.0e")),
    B15 = expect_badarg(?MODULE:id("1.0e++3")),
    B16 = expect_badarg(?MODULE:id("+.5")),
    B17 = expect_badarg(?MODULE:id("-1.")),
    B18 = expect_badarg(?MODULE:id("1.0f")),
    B19 = expect_badarg(?MODULE:id(".")),
    B20 = expect_badarg(?MODULE:id("+")),
    B21 = expect_badarg(?MODULE:id("")),
    B22 = expect_badarg(?MODULE:id("\t1.0")),
    B23 = expect_badarg(?MODULE:id("1.0\n")),

    NewValid = V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10,
    NewBadarg =
        B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 +
            B11 + B12 + B13 + B14 + B15 + B16 + B17 + B18 + B19 + B20 +
            B21 + B22 + B23,

    R0 + (NewValid + NewBadarg) * 512.

list2float(F) ->
    try ?MODULE:id(erlang:list_to_float(F)) of
        Res -> ?MODULE:id(Res)
    catch
        error:badarg -> 0;
        _:_ -> -10000000
    end.

expect_badarg(F) ->
    try ?MODULE:id(erlang:list_to_float(F)) of
        _Res -> 0
    catch
        error:badarg -> 1;
        _:_ -> 0
    end.

float_cmp(F1, F2) ->
    case abs(?MODULE:id(F1) - ?MODULE:id(F2)) < ?MODULE:id(0.00000000001) of
        true -> 1;
        false -> 0
    end.

id(I) when is_float(I) ->
    I;
id(I) when is_list(I) ->
    I;
id(I) when is_tuple(I) ->
    I.
