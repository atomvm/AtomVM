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

-module(floatmul).

-export([start/0, id/1]).

start() ->
    ok = test_mul_bif(),
    ok = test_mul_isfloat(),
    0.

test_mul_bif() ->
    50 = to_int(mul_bif(id(100), id(0.5))),
    ok.

test_mul_isfloat() ->
    50 = to_int(mul_isfloat(id(100.0), id(0.5))),
    ok.

mul_bif(A, B) ->
    try id(A) * id(B) of
        C -> C
    catch
        error:badarith -> fail_badarith;
        _:_ -> fail_other_ex
    end.

id(I) ->
    I.

mul_isfloat(A, B) when is_float(A) andalso is_float(B) ->
    try A * B of
        C -> C
    catch
        error:badarith -> fail_badarith;
        _:_ -> fail_other_ex
    end.

to_int(A) ->
    round(A).
