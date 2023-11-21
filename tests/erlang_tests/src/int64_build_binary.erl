%
% This file is part of AtomVM.
%
% Copyright 2023 Davide Bettio <davide@uninstall.it>
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

-module(int64_build_binary).
-export([start/0, build/1]).

start() ->
    Bin = make_map(3940753902, 3186666752, 7),
    sum_all(binary_to_list(Bin), 1, 0) - 8559.

sum_all([], _N, Acc) ->
    Acc;
sum_all([H | T], N, Acc) ->
    sum_all(T, N + 1, Acc + N * H).

make_map(A, B, C) ->
    ?MODULE:build(#{a64 => A, b32 => B, c => C, d => 0}).

build(#{a64 := A64, b32 := B32}) ->
    <<A64:64/little-unsigned, B32:32/little-unsigned, 0:32>>.
