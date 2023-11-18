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

-module(test_crypto_strong_rand_bytes).
-export([start/0]).

start() ->
    TestScore = times((fun() -> do_test() end), 10, 0),
    if
        TestScore >= 9 -> 1;
        true -> 0
    end.

do_test() ->
    Bin = crypto:strong_rand_bytes(4),
    RandomTuple = binary_to_tuple(Bin),
    all_equal(RandomTuple).

all_equal({A, B, C, D}) when (A =/= B) and (B =/= C) and (C =/= D) ->
    0;
all_equal(_) ->
    1.

binary_to_tuple(B) ->
    L = erlang:binary_to_list(B),
    erlang:list_to_tuple(L).

times(_Fun, 0, Acc) ->
    Acc;
times(Fun, N, Acc) ->
    times(Fun, N - 1, Acc + Fun()).
