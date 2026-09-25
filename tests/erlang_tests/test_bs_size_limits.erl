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

%% Sizes that bs_create_bin cannot represent must be reported as system_limit,
%% and the interpreter and the JIT must agree on where that boundary is. This is
%% not run against BEAM: it builds segments one at a time and aborts the whole
%% node trying to allocate them, rather than rejecting the total up front.
-module(test_bs_size_limits).

-export([start/0, id/1]).

start() ->
    ok = test_representable(),
    ok = test_total_overflows_word(),
    ok = test_large_but_representable(),
    0.

%% An ordinary construction with runtime sizes is unaffected by the checks.
test_representable() ->
    {ok, 80} = build_8(id(10)),
    {ok, 400} = build_40(id(10)),
    ok.

%% 40 segments of nearly the largest small integer: the total wraps a machine
%% word, so no allocation could ever match it.
test_total_overflows_word() ->
    system_limit = build_40(id(max_segment_bits())),
    ok.

%% 16 of the same segments: the total is huge but does not wrap and stays within
%% the representable range. Whether it can be allocated depends on the platform,
%% but it must not be rejected as unrepresentable -- that is the boundary the
%% interpreter and the JIT have to agree on.
test_large_but_representable() ->
    case build_16(id(max_segment_bits())) of
        {ok, _} -> ok;
        out_of_memory -> ok
    end.

%% Just below the largest small integer, so the size is not a boxed integer.
max_segment_bits() ->
    (1 bsl (word_bits() - 5)) - 1.

word_bits() ->
    case erlang:system_info(wordsize) of
        4 -> 32;
        8 -> 64
    end.

build_8(N) ->
    try <<0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N>> of
        R -> {ok, bit_size(R)}
    catch
        error:E -> E
    end.

build_16(N) ->
    try <<0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N>> of
        R -> {ok, bit_size(R)}
    catch
        error:E -> E
    end.

build_40(N) ->
    try
        <<0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N,
            0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N, 0:N,
            0:N, 0:N, 0:N, 0:N, 0:N>>
    of
        R -> {ok, bit_size(R)}
    catch
        error:E -> E
    end.

id(X) ->
    X.
