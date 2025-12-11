%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_fp_allocate_heap_zero).
-export([start/0]).

start() ->
    R = e(0.1, 10000, 0.0),
    ok = assert_float_range(R, 2183.7, 2183.9),
    0.

e(_Step, 0, Acc) ->
    Acc;
e(Step, N, Acc) when is_float(Step) andalso is_integer(N) andalso is_float(Acc) ->
    R = f(Step * N),
    e(Step, N - 1, Acc + math:sqrt(R * R) * Step).

f(T) when is_float(T) ->
    0.3 + 3.2 * math:sin(T) + 0.7 * math:sin(2 * T) + 0.2 * math:sin(4 * T) + 1.2 * math:sin(8 * T).

assert_float_range(R, Min, Max) when R < Max andalso R > Min -> ok;
assert_float_range(R, _Min, _Max) -> {out_of_range, R}.
