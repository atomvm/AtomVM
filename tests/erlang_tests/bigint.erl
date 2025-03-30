%
% This file is part of AtomVM.
%
% Copyright 2025 Davide Bettio <davide@uninstall.it>
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

-module(bigint).
-export([start/0, mul/2, shrink/0, pow/2, twice/1, fact/1, get_machine_atom/0, expect_overflow/1]).

start() ->
    test_mul().

test_mul() ->
    Expected_INT64_MIN = ?MODULE:pow(-2, 63),
    Expected_INT64_MIN = ?MODULE:shrink(),
    A = ?MODULE:mul(16#10101010CAFECAFE, 16#AABB),
    Square = ?MODULE:mul(A, A),
    <<"2559181265480533323615999200984578944503596644">> = erlang:integer_to_binary(Square),

    B = ?MODULE:mul(16#10101010CAFECAFE, 16#AABBCCDD),
    C = ?MODULE:mul(-(16#17322539CAFECAFE), 16#A2CBFCDD),
    D = ?MODULE:mul(16#19171411CAFECAFE, -(16#AF8BCCFD)),
    E = ?MODULE:mul(-(16#34143919CAFECAFE), -(16#8C8BCCED)),

    F = ?MODULE:mul(16#34143919CAFECAFE, 16#1234CAFE5678CAFE),
    G = ?MODULE:mul(-(16#34143919CAFECAFE), 16#1234CAFE5678CAFE),
    H = ?MODULE:twice(?MODULE:twice(G)),

    <<"3315418878780451855276287302">> = erlang:integer_to_binary(B),
    <<"-4565164722186152120719328582">> = erlang:integer_to_binary(C),
    <<"-5324687047716540217489556742">> = erlang:integer_to_binary(D),
    <<"8848732046695083633938421030">> = erlang:integer_to_binary(E),
    <<"4923137486833276011090373091921613828">> = erlang:integer_to_binary(F),
    <<"-4923137486833276011090373091921613828">> = erlang:integer_to_binary(G),
    <<"-19692549947333104044361492367686455312">> = erlang:integer_to_binary(H),

    0 = ?MODULE:mul(0, E),
    0 = ?MODULE:mul(0, H),

    INT255_MIN = ?MODULE:pow(-2, 255),
    ok = ?MODULE:expect_overflow(fun() -> ?MODULE:twice(INT255_MIN) end),
    ok = ?MODULE:expect_overflow(fun() -> ?MODULE:mul(INT255_MIN, -1) end),
    ok = ?MODULE:expect_overflow(fun() -> ?MODULE:mul(-1, INT255_MIN) end),
    <<"-57896044618658097711785492504343953926634992332820282019728792003956564819968">> = erlang:integer_to_binary(
        INT255_MIN
    ),
    erlang:display(INT255_MIN),

    Fact55 = ?MODULE:fact(55),
    <<"12696403353658275925965100847566516959580321051449436762275840000000000000">> = erlang:integer_to_binary(
        Fact55
    ),

    ?MODULE:mul(0, INT255_MIN) + ?MODULE:mul(INT255_MIN, 0).

mul(A, B) ->
    A * B.

shrink() ->
    S1 = ?MODULE:mul(4611686018427387904, 2),
    S2 = ?MODULE:mul(-1, S1),
    S2.

pow(_A, 0) ->
    1;
pow(A, N) ->
    A * pow(A, N - 1).

twice(N) ->
    2 * N.

fact(0) ->
    1;
fact(N) when N rem 2 == 0 ->
    N * fact(N - 1);
fact(N) when N rem 2 == 1 ->
    fact(N - 1) * N.

expect_overflow(OvfFun) ->
    Machine = ?MODULE:get_machine_atom(),
    try {Machine, OvfFun()} of
        {beam, I} when is_integer(I) -> ok;
        {atomvm, Result} -> {unexpected_result, Result}
    catch
        error:overflow -> ok;
        _:E -> {unexpected_error, E}
    end.

get_machine_atom() ->
    case erlang:system_info(machine) of
        "BEAM" -> beam;
        _ -> atomvm
    end.
