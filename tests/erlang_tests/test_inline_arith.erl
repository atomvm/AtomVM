%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(test_inline_arith).

-export([start/0, add_small/1, sub_small/1, add_large/1, sub_large/1]).

% Test inline addition with safe ranges - SHOULD BE INLINED
add_small(X) when is_integer(X), X >= 0, X < 100 ->
    X + 20.

% Test inline subtraction with safe ranges - SHOULD BE INLINED
sub_small(X) when is_integer(X), X >= 50, X < 150 ->
    X - 30.

% Test addition with large range - SHOULD NOT BE INLINED (uses BIF)
add_large(X) when is_integer(X), X >= 0, X < (1 bsl 60) ->
    X + 1.

% Test subtraction with large range - SHOULD NOT BE INLINED (uses BIF)
sub_large(X) when is_integer(X), X >= -(1 bsl 60), X < 100 ->
    X - 1.

start() ->
    % Test safe addition - should be inlined
    20 = ?MODULE:add_small(0),
    50 = ?MODULE:add_small(30),
    119 = ?MODULE:add_small(99),

    % Test safe subtraction - should be inlined
    20 = ?MODULE:sub_small(50),
    70 = ?MODULE:sub_small(100),
    119 = ?MODULE:sub_small(149),

    % Test large addition - not inlined, uses BIF (but should still work correctly)
    % Using values near the upper bound but within the guard range
    1 = ?MODULE:add_large(0),
    100000001 = ?MODULE:add_large(100000000),
    (1 bsl 59) = ?MODULE:add_large((1 bsl 59) - 1),

    % Test large subtraction - not inlined, uses BIF (but should still work correctly)
    % Using values near the lower bound but within the guard range
    -1 = ?MODULE:sub_large(0),
    -100000001 = ?MODULE:sub_large(-100000000),
    -(1 bsl 59) - 1 = ?MODULE:sub_large(-(1 bsl 59)),

    0.
