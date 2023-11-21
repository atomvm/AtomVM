%
% This file is part of AtomVM.
%
% Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

-module(floatdiv).

-export([start/0, id/1]).

start() ->
    ok = test_div_bif(),
    ok = test_div_isfloat(),
    ok = test_div_zero_bif(),
    ok = test_div_zero_isfloat(),
    ok = test_div_ovf_bif(),
    ok = test_div_ovf_isfloat(),
    ok = test_div_udf_bif(),
    ok = test_div_udf_isfloat(),
    0.

test_div_bif() ->
    2 = to_int(div_bif(id(4.2), id(2.1))),
    ok.

test_div_isfloat() ->
    2 = to_int(div_isfloat(id(4.2), id(2.1))),
    ok.

test_div_zero_bif() ->
    try
        div_bif(id(4.2), id(0.0)),
        fail_no_ex
    catch
        error:badarith -> ok
    end.

test_div_zero_isfloat() ->
    try
        div_isfloat(id(4.2), id(0.0)),
        fail_no_ex
    catch
        error:badarith -> ok
    end.

test_div_ovf_bif() ->
    try
        div_bif(max_float(), 0.1),
        fail_no_ex
    catch
        error:badarith -> ok
    end.

test_div_ovf_isfloat() ->
    try
        div_isfloat(max_float(), 0.1),
        fail_no_ex
    catch
        error:badarith -> ok
    end.

test_div_udf_bif() ->
    R = div_bif(min_normal_positive_float(), 1.0e25),
    0 = to_int(R),
    ok.

test_div_udf_isfloat() ->
    R = div_isfloat(min_normal_positive_float(), 1.0e25),
    0 = to_int(R),
    ok.

div_bif(A, B) ->
    id(A) / id(B).

div_isfloat(A, B) when is_float(A) andalso is_float(B) ->
    A / B.

to_int(A) ->
    round(A).

id(I) ->
    I.

max_float() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            1.7976931348623157e308;
        "ATOM" ->
            case erlang:system_info(avm_floatsize) of
                4 -> 3.4028234664e38;
                8 -> 1.7976931348623157e308
            end
    end.

min_normal_positive_float() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            2.2250738585072014e-308;
        "ATOM" ->
            case erlang:system_info(avm_floatsize) of
                4 -> 1.1754943508e-38;
                8 -> 2.2250738585072014e-308
            end
    end.
