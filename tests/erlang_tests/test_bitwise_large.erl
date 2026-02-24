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

-module(test_bitwise_large).

-export([start/0, id/1, band_regs/2, bor_regs/2, bxor_regs/2]).

%% Test bitwise operations with large constants (> 32 bits when tagged)
%% to ensure the JIT handles them correctly.
start() ->
    ok = test_bor(id(0)),
    ok = test_band(id(16#FFFFFFFFFF)),
    ok = test_bxor(id(0)),

    % Register-register bitwise with small ranges
    0 = ?MODULE:band_regs(0, 0),
    5 = ?MODULE:band_regs(7, 13),
    255 = ?MODULE:band_regs(255, 255),
    0 = ?MODULE:band_regs(170, 85),

    7 = ?MODULE:bor_regs(3, 5),
    255 = ?MODULE:bor_regs(170, 85),
    0 = ?MODULE:bor_regs(0, 0),

    6 = ?MODULE:bxor_regs(3, 5),
    255 = ?MODULE:bxor_regs(170, 85),
    0 = ?MODULE:bxor_regs(42, 42),

    0.

test_bor(X) when is_integer(X), X >= 0, X < 16#100000000 ->
    %% 16#91111111 > 16#7FFFFFFF when tagged (shifted left 4 + tag)
    Result = X bor 16#91111111,
    case Result of
        16#91111111 -> ok;
        _ -> {error, bor_result, Result}
    end.

test_band(X) when is_integer(X), X >= 0, X < 16#10000000000 ->
    Result = X band 16#91111111,
    case Result of
        16#91111111 -> ok;
        _ -> {error, band_result, Result}
    end.

test_bxor(X) when is_integer(X), X >= 0, X < 16#100000000 ->
    Result = X bxor 16#91111111,
    case Result of
        16#91111111 -> ok;
        _ -> {error, bxor_result, Result}
    end.

id(X) -> X.

% Register-register bitwise operations with small ranges (inlined by JIT)
band_regs(X, Y) when is_integer(X), X >= 0, X < 256, is_integer(Y), Y >= 0, Y < 256 ->
    X band Y.

bor_regs(X, Y) when is_integer(X), X >= 0, X < 256, is_integer(Y), Y >= 0, Y < 256 ->
    X bor Y.

bxor_regs(X, Y) when is_integer(X), X >= 0, X < 256, is_integer(Y), Y >= 0, Y < 256 ->
    X bxor Y.
