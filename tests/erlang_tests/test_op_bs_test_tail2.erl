%
% This file is part of AtomVM.
%
% Copyright 2026 Davide Bettio <davide@uninstall.it>
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

-module(test_op_bs_test_tail2).

%% Since OTP 27, without SSA optimizations the compiler doesn't emit
%% bs_match, and matches ending with a fixed-size segment use bs_test_tail2
%% instead. Unlike no_bs_match, no_ssa_opt is still supported by OTP 29,
%% and beam_debug_info (OTP 28+) enables it too.
-compile([no_ssa_opt]).

-export([start/0, id/1]).

start() ->
    {1, 2} = pair(?MODULE:id(<<1, 2>>)),
    error = pair(?MODULE:id(<<1>>)),
    error = pair(?MODULE:id(<<1, 2, 3>>)),
    error = pair(?MODULE:id(<<1, 2, 3:4>>)),
    {1, 2} = pair(?MODULE:id(<<0:4, 1, 2>>), 4),
    error = pair(?MODULE:id(<<0:4, 1, 2, 3:4>>), 4),
    0.

pair(<<A:8, B:8>>) -> {A, B};
pair(_) -> error.

pair(Bin, Skip) ->
    case Bin of
        <<_:Skip, A:8, B:8>> -> {A, B};
        _ -> error
    end.

id(X) -> X.
