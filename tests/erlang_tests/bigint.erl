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
-export([
    start/0,
    mul/2,
    shrink/0,
    pow/2,
    sort/1,
    twice/1,
    fact/1,
    lit_ovf1/0,
    lit_ovf2/0,
    divtrunc/2,
    the_out_of_order_list/0,
    the_ordered_list/0,
    get_machine_atom/0,
    expect_error/2,
    expect_overflow/1,
    is_integer_helper/1,
    is_number_helper/1,
    classify1/1,
    classify2/1,
    id/1,
    idB/1,
    t2/2,
    fst/1
]).

%
% IMPORTANT NOTE
% AtomVM supports up to 256-bit integers with an additional sign bit stored outside the numeric
% payload, allowing for efficient representation of both signed and unsigned values without using
% two's complement encoding. So INT_MAX = -INT_MIN, that is:
% 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
%

start() ->
    test_mul() +
        test_div() +
        test_rem() +
        test_add() +
        test_sub() +
        test_abs() +
        test_neg() +
        parse_bigint() +
        test_integer_to_list() +
        test_integer_from_list() +
        test_cmp() +
        conv_to_from_float() +
        external_term_decode() +
        test_big_literals() +
        test_is_integer() +
        test_is_number() +
        test_gt_lt_guards() +
        to_external_term() +
        test_band() +
        test_bxor() +
        test_bor() +
        test_bsl() +
        test_bsr() +
        test_bnot().

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

    % Note: it is not possible to reach min and max values using just multiplications
    % since min and max are +- (2^256 - 1)

    P255_MIN = ?MODULE:pow(-2, 255),
    <<"-57896044618658097711785492504343953926634992332820282019728792003956564819968">> = erlang:integer_to_binary(
        P255_MIN
    ),
    P255_MAX_A = ?MODULE:mul(P255_MIN, -1),
    P255_MAX_B = ?MODULE:mul(-1, P255_MIN),
    <<"57896044618658097711785492504343953926634992332820282019728792003956564819968">> = erlang:integer_to_binary(
        P255_MAX_A
    ),
    <<"57896044618658097711785492504343953926634992332820282019728792003956564819968">> = erlang:integer_to_binary(
        P255_MAX_B
    ),

    ok = ?MODULE:expect_overflow(fun() -> ?MODULE:twice(P255_MIN) end),
    ok = ?MODULE:expect_overflow(fun() -> ?MODULE:mul(P255_MIN, -2) end),
    ok = ?MODULE:expect_overflow(fun() -> ?MODULE:mul(2, P255_MIN) end),
    erlang:display(P255_MIN),

    Fact55 = ?MODULE:fact(55),
    <<"12696403353658275925965100847566516959580321051449436762275840000000000000">> = erlang:integer_to_binary(
        Fact55
    ),

    ?MODULE:mul(0, P255_MIN) + ?MODULE:mul(P255_MIN, 0) + ?MODULE:mul(0, P255_MAX_A) +
        ?MODULE:mul(P255_MAX_B, 0).

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

test_div() ->
    Int0 = erlang:binary_to_integer(
        ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    Int1 = erlang:binary_to_integer(?MODULE:id(<<"ABCDEF123456789FFAABBCCDDEE11223">>), 16),
    Int2 = erlang:binary_to_integer(?MODULE:id(<<"-FFFFFFFFFFFFFFFF">>), 16),
    Int3 = erlang:binary_to_integer(
        ?MODULE:id(<<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),

    <<"17D74FD225B3F8B4E8DB72B81BE0416D2">> = erlang:integer_to_binary(
        ?MODULE:id(Int0) div ?MODULE:id(Int1), 16
    ),

    <<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(
        ?MODULE:id(Int0) div ?MODULE:id(-1), 16
    ),

    1 = ?MODULE:id(Int0) div ?MODULE:id(Int0),
    1 = ?MODULE:id(Int1) div ?MODULE:id(Int1),
    1 = ?MODULE:id(Int2) div ?MODULE:id(Int2),
    -1 = ?MODULE:id(Int0) div ?MODULE:id(Int3),
    -1 = ?MODULE:id(Int3) div ?MODULE:id(Int0),
    0 = ?MODULE:id(Int1) div ?MODULE:id(Int0),
    0 = ?MODULE:id(0) div ?MODULE:id(Int0),
    0 = ?MODULE:id(Int1) div ?MODULE:id(Int3),

    32894 =
        (((((((?MODULE:id(Int0) div ?MODULE:id(2)) div ?MODULE:id(123456)) div
            ?MODULE:id(123456789)) div ?MODULE:id(9876543210)) div ?MODULE:id(1125899906842601)) div
            ?MODULE:id(1125899906841712)) div ?MODULE:id(9223372036854773330)),

    -2196990 =
        (((((((?MODULE:id(Int3) div ?MODULE:id(3)) div ?MODULE:id(123431)) div
            ?MODULE:id(123256789)) div ?MODULE:id(9876543217)) div ?MODULE:id(1125899916842637)) div
            ?MODULE:id(1125899906841719)) div ?MODULE:id(92233720368547733)),

    <<"8000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(-16#8000000000000000) div ?MODULE:id(-1), 16
    ),

    <<"FFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(
        ?MODULE:id(Int2) div ?MODULE:id(-1), 16
    ),

    ok = expect_error(badarith, fun() -> Int1 div ?MODULE:id(0) end),

    0.

test_rem() ->
    Int0 = erlang:binary_to_integer(
        ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    Int1 = erlang:binary_to_integer(
        ?MODULE:id(<<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    Int2 = erlang:binary_to_integer(?MODULE:id(<<"ABCDEF123456789FFAABBCCDDEE11223">>), 16),
    Int3 = erlang:binary_to_integer(?MODULE:id(<<"-FFFFFFFFFFFFFFFF">>), 16),

    Int4 = erlang:binary_to_integer(
        ?MODULE:id(<<"1AD15A70023DBFE3CF869EFD994596BDF42A4BE8A164825CB81420FBA070BDEF">>), 16
    ),
    Int5 = erlang:binary_to_integer(
        ?MODULE:id(<<"77DEF52A78035143AD8561489A0108EDFB1741FE95172248814AE0A8BD2AEBB">>), 16
    ),
    Int6 = erlang:binary_to_integer(
        ?MODULE:id(<<"-4531A41167802967085EBCC1B0AA2843C1A02C4959E911636CE52ED2FD77EBE6">>), 16
    ),
    Int7 = erlang:binary_to_integer(
        ?MODULE:id(<<"-E8F8DE9724DC489EE5033E06E5032BB883968334C717C819DA9BD314758B0640">>), 16
    ),
    Int8 = erlang:binary_to_integer(?MODULE:id(<<"E3AE0EA63AE33EA79B071316BC9A7F1B">>), 16),
    Int9 = erlang:binary_to_integer(?MODULE:id(<<"A4EF35909EA6E73C93C66B937541696A9C">>), 16),

    0 = ?MODULE:id(0) rem Int0,
    0 = Int0 rem Int0,
    0 = Int0 rem Int1,
    0 = Int1 rem Int0,
    <<"45BABAFD7AF162B182C7E25A91441D49">> = erlang:integer_to_binary(Int0 rem Int2, 16),
    <<"-45BABAFD7AF162B182C7E25A91441D49">> = erlang:integer_to_binary(Int1 rem Int2, 16),

    <<"A679ABE013378AC3">> = erlang:integer_to_binary(Int2 rem Int3, 16),
    <<"-FFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(Int3 rem Int2, 16),
    0 = ?MODULE:id(0) rem Int3,
    <<"ABCDEF123456789FFAABBCCDDEE11223">> = erlang:integer_to_binary(Int2 rem Int4, 16),
    <<"4F5625F74C45E7ABBD9BA68A6A5E39D3">> = erlang:integer_to_binary(Int4 rem Int2, 16),
    <<"-FFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(Int3 rem Int4, 16),
    <<"96966651DD5896ED">> = erlang:integer_to_binary(Int4 rem Int3, 16),

    <<"4578C780BBD20A71EFD9CBFFC6565115515EF88E5702BEF1FD616DBFCF8B1BE">> = erlang:integer_to_binary(
        Int4 rem Int5, 16
    ),
    <<"-4531A41167802967085EBCC1B0AA2843C1A02C4959E911636CE52ED2FD77EBE6">> = erlang:integer_to_binary(
        Int6 rem Int7, 16
    ),
    <<"E3AE0EA63AE33EA79B071316BC9A7F1B">> = erlang:integer_to_binary(Int8 rem Int9, 16),

    ok = expect_error(badarith, fun() -> Int0 rem ?MODULE:id(0) end),
    ok = expect_error(badarith, fun() -> Int1 rem ?MODULE:id(0) end),

    0.

test_add() ->
    Int0 = erlang:binary_to_integer(
        ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE">>), 16
    ),
    Int1 = erlang:binary_to_integer(
        ?MODULE:id(<<"F000000000000000000000000000000000000000000000000000000000000000">>), 16
    ),
    Int2 = erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFFFFFFFFFF">>), 16),
    Int3 = erlang:binary_to_integer(?MODULE:id(<<"-FFFFFFFFFFFFFFFF">>), 16),
    Int4 = erlang:binary_to_integer(?MODULE:id(<<"ABCDEF0123456789">>), 16),
    Int5 = erlang:binary_to_integer(?MODULE:id(<<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16),
    Int6 = erlang:binary_to_integer(?MODULE:id(<<"ABCDEF123456789FFAABBCCDDEE11223">>), 16),
    Int7 = erlang:binary_to_integer(
        ?MODULE:id(<<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),

    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(
        ?MODULE:id(Int0) + ?MODULE:id(1), 16
    ),
    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Int0) + ?MODULE:id(1)) + ?MODULE:id(0), 16
    ),
    <<"F00000000000000000000000000000000000000000000000FFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(
        ?MODULE:id(Int1) + ?MODULE:id(Int2), 16
    ),
    <<"EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000001">> = erlang:integer_to_binary(
        Int1 + Int3, 16
    ),

    0 = Int2 + Int3,

    <<"-543210FEDCBA9876">> = erlang:integer_to_binary(?MODULE:id(Int3) + ?MODULE:id(Int4), 16),
    <<"-543210EDCBA9876005544332211EEDDC">> = erlang:integer_to_binary(Int5 + Int6, 16),

    %% Both positive (always positive result)
    M1 = erlang:binary_to_integer(
        ?MODULE:id(<<"999999999999999999999999999999999999999999999999">>)
    ),
    N1 = erlang:binary_to_integer(
        ?MODULE:id(<<"123456789012345678901234567890123456789012345678">>)
    ),
    <<"1123456789012345678901234567890123456789012345677">> = erlang:integer_to_binary(
        ?MODULE:id(M1) + ?MODULE:id(N1)
    ),

    M2 = erlang:binary_to_integer(
        ?MODULE:id(<<"500000000000000000000000000000000000000000000000">>)
    ),
    N2 = erlang:binary_to_integer(
        ?MODULE:id(<<"700000000000000000000000000000000000000000000000">>)
    ),
    <<"1200000000000000000000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(M2) + ?MODULE:id(N2)
    ),

    %% m positive, n negative (|m| > |n|, result positive)
    M3 = erlang:binary_to_integer(
        ?MODULE:id(<<"800000000000000000000000000000000000000000000000">>)
    ),
    N3 = erlang:binary_to_integer(
        ?MODULE:id(<<"-300000000000000000000000000000000000000000000000">>)
    ),
    <<"500000000000000000000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(M3) + ?MODULE:id(N3)
    ),

    %% m positive, n negative (|m| < |n|, result negative)
    M4 = erlang:binary_to_integer(
        ?MODULE:id(<<"200000000000000000000000000000000000000000000000">>)
    ),
    N4 = erlang:binary_to_integer(
        ?MODULE:id(<<"-900000000000000000000000000000000000000000000000">>)
    ),
    <<"-700000000000000000000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(M4) + ?MODULE:id(N4)
    ),

    %% m positive, n negative (|m| = |n|, result zero)
    M5 = erlang:binary_to_integer(
        ?MODULE:id(<<"12345678901234567890123456789012345678901234567890">>)
    ),
    N5 = erlang:binary_to_integer(
        ?MODULE:id(<<"-12345678901234567890123456789012345678901234567890">>)
    ),
    <<"0">> = erlang:integer_to_binary(?MODULE:id(M5) + ?MODULE:id(N5)),

    %% m negative, n positive (|m| < |n|, result positive)
    M6 = erlang:binary_to_integer(
        ?MODULE:id(<<"-400000000000000000000000000000000000000000000000">>)
    ),
    N6 = erlang:binary_to_integer(
        ?MODULE:id(<<"600000000000000000000000000000000000000000000000">>)
    ),
    <<"200000000000000000000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(M6) + ?MODULE:id(N6)
    ),

    %% m negative, n positive (|m| > |n|, result negative)
    M7 = erlang:binary_to_integer(
        ?MODULE:id(<<"-777777777777777777777777777777777777777777777777">>)
    ),
    N7 = erlang:binary_to_integer(
        ?MODULE:id(<<"111111111111111111111111111111111111111111111111">>)
    ),
    <<"-666666666666666666666666666666666666666666666666">> = erlang:integer_to_binary(
        ?MODULE:id(M7) + ?MODULE:id(N7)
    ),

    %% Both negative (always negative)
    M8 = erlang:binary_to_integer(
        ?MODULE:id(<<"-900000000000000000000000000000000000000000000000">>)
    ),
    N8 = erlang:binary_to_integer(
        ?MODULE:id(<<"-200000000000000000000000000000000000000000000000">>)
    ),
    <<"-1100000000000000000000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(M8) + ?MODULE:id(N8)
    ),

    %% Both negative (large numbers)
    M9 = erlang:binary_to_integer(
        ?MODULE:id(<<"-555555555555555555555555555555555555555555555555">>)
    ),
    N9 = erlang:binary_to_integer(
        ?MODULE:id(<<"-444444444444444444444444444444444444444444444444">>)
    ),
    <<"-999999999999999999999999999999999999999999999999">> = erlang:integer_to_binary(
        ?MODULE:id(M9) + ?MODULE:id(N9)
    ),

    %% Misc tests

    M10 = erlang:binary_to_integer(
        ?MODULE:id(<<"9999999999999999999999999999999999999999999999999999999999">>)
    ),
    N10 = erlang:binary_to_integer(?MODULE:id(<<"1">>)),
    <<"10000000000000000000000000000000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(M10) + ?MODULE:id(N10)
    ),

    M11 = erlang:binary_to_integer(
        ?MODULE:id(<<"9999999999999999999999999999999999999999999999999999999999">>)
    ),
    N11 = erlang:binary_to_integer(
        ?MODULE:id(<<"-9999999999999999999999999999999999999999999999999999999998">>)
    ),
    <<"1">> = erlang:integer_to_binary(?MODULE:id(M11) + ?MODULE:id(N11)),

    M12 = erlang:binary_to_integer(
        ?MODULE:id(<<"-1234567890123456789012345678901234567890123456789012345678901234567890">>)
    ),
    N12 = erlang:binary_to_integer(
        ?MODULE:id(<<"1234567890123456789012345678901234567890123456789012345678901234567891">>)
    ),
    <<"1">> = erlang:integer_to_binary(?MODULE:id(M12) + ?MODULE:id(N12)),

    16#3501FEDCB2152350 =
        erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFFFFFFFFFF">>), 16) +
            erlang:binary_to_integer(?MODULE:id(<<"-CAFE01234DEADCAF">>), 16),

    -16#3501FEDCB2152350 =
        erlang:binary_to_integer(?MODULE:id(<<"CAFE01234DEADCAF">>), 16) +
            erlang:binary_to_integer(?MODULE:id(<<"-FFFFFFFFFFFFFFFF">>), 16),

    16#3501FEDCB2152350 =
        erlang:binary_to_integer(?MODULE:id(<<"-CAFE01234DEADCAF">>), 16) +
            erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFFFFFFFFFF">>), 16),

    <<"8000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(16#7FFFFFFFFFFFFFFF) + ?MODULE:id(1), 16
    ),
    <<"-8000000000000001">> = erlang:integer_to_binary(
        ?MODULE:id(-16#8000000000000000) + ?MODULE:id(-1), 16
    ),

    ok = ?MODULE:expect_overflow(fun() -> Int0 + ?MODULE:id(2) end),
    ok = ?MODULE:expect_overflow(fun() -> Int0 + ?MODULE:id(16#7FFFFFFFFFFFFFFF) end),
    ok = ?MODULE:expect_overflow(fun() ->
        Int0 + erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFFFFFFFFFF">>), 16)
    end),
    ok = ?MODULE:expect_overflow(fun() -> ?MODULE:id(Int0) + ?MODULE:id(Int0) end),
    ok = ?MODULE:expect_overflow(fun() -> Int7 + ?MODULE:id(-1) end),
    ok = ?MODULE:expect_overflow(fun() -> Int5 + Int7 end),

    0.

test_sub() ->
    Int0 = erlang:binary_to_integer(
        ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE">>), 16
    ),
    Int1 = erlang:binary_to_integer(
        ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    Int2 = erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFFFFFFFFFF">>), 16),
    Int3 = erlang:binary_to_integer(?MODULE:id(<<"-FFFFFFFFFFFFFFFF">>), 16),
    Int4 = erlang:binary_to_integer(?MODULE:id(<<"ABCDEF0123456789">>), 16),
    Int5 = erlang:binary_to_integer(?MODULE:id(<<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16),
    Int6 = erlang:binary_to_integer(?MODULE:id(<<"ABCDEF123456789FFAABBCCDDEE11223">>), 16),

    <<"1">> = erlang:integer_to_binary(?MODULE:id(Int1) - ?MODULE:id(Int0)),
    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE">> = erlang:integer_to_binary(
        ?MODULE:id(Int1) - ?MODULE:id(1), 16
    ),
    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE">> = erlang:integer_to_binary(
        Int1 - 1, 16
    ),
    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000">> = erlang:integer_to_binary(
        Int1 - Int2, 16
    ),
    <<"-1ABCDEF0123456788">> = erlang:integer_to_binary(?MODULE:id(Int3) - ?MODULE:id(Int4), 16),
    <<"-1ABCDEF123456789FFAABBCCDDEE11222">> = erlang:integer_to_binary(Int5 - Int6, 16),

    %% Case 1: Both positive (equal, result zero)
    M3 = erlang:binary_to_integer(
        ?MODULE:id(<<"12345678901234567890123456789012345678901234567890">>)
    ),
    N3 = erlang:binary_to_integer(
        ?MODULE:id(<<"12345678901234567890123456789012345678901234567890">>)
    ),
    <<"0">> = erlang:integer_to_binary(?MODULE:id(M3) - ?MODULE:id(N3)),

    %% Case 2: m positive, n negative (result always positive)
    M4 = erlang:binary_to_integer(
        ?MODULE:id(<<"800000000000000000000000000000000000000000000000">>)
    ),
    N4 = erlang:binary_to_integer(
        ?MODULE:id(<<"-300000000000000000000000000000000000000000000000">>)
    ),
    <<"1100000000000000000000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(M4) - ?MODULE:id(N4)
    ),

    %% Case 2: m positive, n negative (large numbers)
    M5 = erlang:binary_to_integer(
        ?MODULE:id(<<"98765432109876543210987654321098765432109876543210">>)
    ),
    N5 = erlang:binary_to_integer(
        ?MODULE:id(<<"-11111111111111111111111111111111111111111111111111">>)
    ),
    <<"109876543220987654322098765432209876543220987654321">> = erlang:integer_to_binary(
        ?MODULE:id(M5) - ?MODULE:id(N5)
    ),

    %% Case 3: m negative, n positive (result always negative)
    M6 = erlang:binary_to_integer(
        ?MODULE:id(<<"-600000000000000000000000000000000000000000000000">>)
    ),
    N6 = erlang:binary_to_integer(
        ?MODULE:id(<<"400000000000000000000000000000000000000000000000">>)
    ),
    <<"-1000000000000000000000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(M6) - ?MODULE:id(N6)
    ),

    %% Case 3: m negative, n positive (large numbers)
    M7 = erlang:binary_to_integer(
        ?MODULE:id(<<"-55555555555555555555555555555555555555555555555555">>)
    ),
    N7 = erlang:binary_to_integer(
        ?MODULE:id(<<"44444444444444444444444444444444444444444444444444">>)
    ),
    <<"-99999999999999999999999999999999999999999999999999">> = erlang:integer_to_binary(
        ?MODULE:id(M7) - ?MODULE:id(N7)
    ),

    %% Case 4: Both negative (|m| > |n|, result negative)
    M8 = erlang:binary_to_integer(
        ?MODULE:id(<<"-900000000000000000000000000000000000000000000000">>)
    ),
    N8 = erlang:binary_to_integer(
        ?MODULE:id(<<"-200000000000000000000000000000000000000000000000">>)
    ),
    <<"-700000000000000000000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(M8) - ?MODULE:id(N8)
    ),

    %% Case 4: Both negative (|m| < |n|, result positive)
    M9 = erlang:binary_to_integer(
        ?MODULE:id(<<"-111111111111111111111111111111111111111111111111">>)
    ),
    N9 = erlang:binary_to_integer(
        ?MODULE:id(<<"-777777777777777777777777777777777777777777777777">>)
    ),
    <<"666666666666666666666666666666666666666666666666">> = erlang:integer_to_binary(
        ?MODULE:id(M9) - ?MODULE:id(N9)
    ),

    %% Case 4: Both negative (equal magnitudes, result zero)
    M10 = erlang:binary_to_integer(
        ?MODULE:id(<<"-123123123123123123123123123123123123123123123123">>)
    ),
    N10 = erlang:binary_to_integer(
        ?MODULE:id(<<"-123123123123123123123123123123123123123123123123">>)
    ),
    <<"0">> = erlang:integer_to_binary(?MODULE:id(M10) - ?MODULE:id(N10)),

    %% Edge case: Large 200+ bit numbers
    M11 = erlang:binary_to_integer(
        ?MODULE:id(<<"1234567890123456789012345678901234567890123456789012345678901234567890">>)
    ),
    N11 = erlang:binary_to_integer(
        ?MODULE:id(<<"1234567890123456789012345678901234567890123456789012345678901234567889">>)
    ),
    <<"1">> = erlang:integer_to_binary(?MODULE:id(M11) - ?MODULE:id(N11)),

    %% Edge case: Mixed signs with very large numbers
    M12 = erlang:binary_to_integer(
        ?MODULE:id(<<"9999999999999999999999999999999999999999999999999999999999">>)
    ),
    N12 = erlang:binary_to_integer(
        ?MODULE:id(<<"-9999999999999999999999999999999999999999999999999999999999">>)
    ),
    <<"19999999999999999999999999999999999999999999999999999999998">> = erlang:integer_to_binary(
        ?MODULE:id(M12) - ?MODULE:id(N12)
    ),

    16#3501FEDCB2152350 =
        erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFFFFFFFFFF">>), 16) -
            erlang:binary_to_integer(?MODULE:id(<<"CAFE01234DEADCAF">>), 16),

    -16#3501FEDCB2152350 =
        erlang:binary_to_integer(?MODULE:id(<<"CAFE01234DEADCAF">>), 16) -
            erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFFFFFFFFFF">>), 16),

    16#3501FEDCB2152350 =
        erlang:binary_to_integer(?MODULE:id(<<"-CAFE01234DEADCAF">>), 16) -
            erlang:binary_to_integer(?MODULE:id(<<"-FFFFFFFFFFFFFFFF">>), 16),

    <<"8000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(16#7FFFFFFFFFFFFFFF) - ?MODULE:id(-1), 16
    ),
    <<"-8000000000000001">> = erlang:integer_to_binary(
        ?MODULE:id(-16#8000000000000000) - ?MODULE:id(1), 16
    ),

    16#7FFFFFFFFFFFFFFF =
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"8000000000000000">>), 16)) -
            ?MODULE:id(1),
    -16#8000000000000000 =
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"-8000000000000001">>), 16)) -
            ?MODULE:id(-1),

    ok = ?MODULE:expect_overflow(fun() -> Int0 - ?MODULE:id(-2) end),
    ok = ?MODULE:expect_overflow(fun() -> Int1 - ?MODULE:id(-1) end),
    ok = ?MODULE:expect_overflow(fun() -> ?MODULE:id(-1) - Int1 end),

    0.

test_abs() ->
    Int0 = erlang:binary_to_integer(
        ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    Int1 = erlang:binary_to_integer(
        ?MODULE:id(<<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    Int2 = erlang:binary_to_integer(
        ?MODULE:id(<<"1AD15A70023DBFE3CF869EFD994596BDF42A4BE8A164825CB81420FBA070BDEF">>), 16
    ),
    Int3 = erlang:binary_to_integer(
        ?MODULE:id(<<"77DEF52A78035143AD8561489A0108EDFB1741FE95172248814AE0A8BD2AEBB">>), 16
    ),
    Int4 = erlang:binary_to_integer(
        ?MODULE:id(<<"-4531A41167802967085EBCC1B0AA2843C1A02C4959E911636CE52ED2FD77EBE6">>), 16
    ),
    Int5 = erlang:binary_to_integer(
        ?MODULE:id(<<"-E8F8DE9724DC489EE5033E06E5032BB883968334C717C819DA9BD314758B0640">>), 16
    ),
    Int6 = erlang:binary_to_integer(?MODULE:id(<<"CAFE01234DEADCAF">>), 16),
    Int7 = erlang:binary_to_integer(?MODULE:id(<<"-CAFE01234DEADCAF">>), 16),

    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(
        abs(Int0), 16
    ),
    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(
        abs(Int1), 16
    ),
    <<"1AD15A70023DBFE3CF869EFD994596BDF42A4BE8A164825CB81420FBA070BDEF">> = erlang:integer_to_binary(
        abs(Int2), 16
    ),
    <<"77DEF52A78035143AD8561489A0108EDFB1741FE95172248814AE0A8BD2AEBB">> = erlang:integer_to_binary(
        abs(Int3), 16
    ),
    <<"4531A41167802967085EBCC1B0AA2843C1A02C4959E911636CE52ED2FD77EBE6">> = erlang:integer_to_binary(
        abs(Int4), 16
    ),
    <<"E8F8DE9724DC489EE5033E06E5032BB883968334C717C819DA9BD314758B0640">> = erlang:integer_to_binary(
        abs(Int5), 16
    ),
    <<"CAFE01234DEADCAF">> = erlang:integer_to_binary(abs(Int6), 16),
    <<"CAFE01234DEADCAF">> = erlang:integer_to_binary(abs(Int7), 16),

    <<"7FFFFFFFFFFFFFFF">> = erlang:integer_to_binary(abs(?MODULE:id(16#7FFFFFFFFFFFFFFF)), 16),
    <<"7FFFFFFFFFFFFFFF">> = erlang:integer_to_binary(abs(?MODULE:id(-16#7FFFFFFFFFFFFFFF)), 16),
    <<"8000000000000000">> = erlang:integer_to_binary(
        abs(erlang:binary_to_integer(?MODULE:id(<<"8000000000000000">>), 16)), 16
    ),
    <<"8000000000000000">> = erlang:integer_to_binary(abs(?MODULE:id(-16#8000000000000000)), 16),

    0.

test_neg() ->
    Int0 = erlang:binary_to_integer(
        ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    Int1 = erlang:binary_to_integer(
        ?MODULE:id(<<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    Int2 = erlang:binary_to_integer(
        ?MODULE:id(<<"1AD15A70023DBFE3CF869EFD994596BDF42A4BE8A164825CB81420FBA070BDEF">>), 16
    ),
    Int3 = erlang:binary_to_integer(
        ?MODULE:id(<<"77DEF52A78035143AD8561489A0108EDFB1741FE95172248814AE0A8BD2AEBB">>), 16
    ),
    Int4 = erlang:binary_to_integer(
        ?MODULE:id(<<"-4531A41167802967085EBCC1B0AA2843C1A02C4959E911636CE52ED2FD77EBE6">>), 16
    ),
    Int5 = erlang:binary_to_integer(
        ?MODULE:id(<<"-E8F8DE9724DC489EE5033E06E5032BB883968334C717C819DA9BD314758B0640">>), 16
    ),
    Int6 = erlang:binary_to_integer(?MODULE:id(<<"CAFE01234DEADCAF">>), 16),
    Int7 = erlang:binary_to_integer(?MODULE:id(<<"-CAFE01234DEADCAF">>), 16),

    <<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(
        -(Int0), 16
    ),
    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(
        -(Int1), 16
    ),
    <<"-1AD15A70023DBFE3CF869EFD994596BDF42A4BE8A164825CB81420FBA070BDEF">> = erlang:integer_to_binary(
        -(Int2), 16
    ),
    <<"-77DEF52A78035143AD8561489A0108EDFB1741FE95172248814AE0A8BD2AEBB">> = erlang:integer_to_binary(
        -(Int3), 16
    ),
    <<"4531A41167802967085EBCC1B0AA2843C1A02C4959E911636CE52ED2FD77EBE6">> = erlang:integer_to_binary(
        -(Int4), 16
    ),
    <<"E8F8DE9724DC489EE5033E06E5032BB883968334C717C819DA9BD314758B0640">> = erlang:integer_to_binary(
        -(Int5), 16
    ),
    <<"-CAFE01234DEADCAF">> = erlang:integer_to_binary(-(Int6), 16),
    <<"CAFE01234DEADCAF">> = erlang:integer_to_binary(-(Int7), 16),

    <<"-7FFFFFFFFFFFFFFF">> = erlang:integer_to_binary(-(?MODULE:id(16#7FFFFFFFFFFFFFFF)), 16),
    <<"7FFFFFFFFFFFFFFF">> = erlang:integer_to_binary(-(?MODULE:id(-16#7FFFFFFFFFFFFFFF)), 16),
    <<"-8000000000000000">> = erlang:integer_to_binary(
        -(erlang:binary_to_integer(?MODULE:id(<<"8000000000000000">>), 16)), 16
    ),
    <<"8000000000000000">> = erlang:integer_to_binary(-(?MODULE:id(-16#8000000000000000)), 16),

    0.

parse_bigint() ->
    PBI = erlang:binary_to_integer(?MODULE:id(<<"1234567892244667788990000000000000000025">>)),
    <<"1234567892244667788990000000000000000025">> = erlang:integer_to_binary(PBI),
    NBI = erlang:binary_to_integer(?MODULE:id(<<"-9234567892244667788990000000000000000025">>)),
    <<"-9234567892244667788990000000000000000025">> = erlang:integer_to_binary(NBI),

    % They are 2^256 - 1 and -(2^256 - 1), that are maximum and minimum supported integers
    % 2-complement representation is not used, so the unsigned part is identical, an additional
    % bit is used for sign, so it is like having a 257 signed bit integer

    INT_MIN_10 = erlang:binary_to_integer(
        ?MODULE:id(
            <<"-115792089237316195423570985008687907853269984665640564039457584007913129639935">>
        )
    ),
    <<"-115792089237316195423570985008687907853269984665640564039457584007913129639935">> = erlang:integer_to_binary(
        INT_MIN_10
    ),
    INT_MAX_10 = erlang:binary_to_integer(
        ?MODULE:id(
            <<"115792089237316195423570985008687907853269984665640564039457584007913129639935">>
        )
    ),
    <<"115792089237316195423570985008687907853269984665640564039457584007913129639935">> = erlang:integer_to_binary(
        INT_MAX_10
    ),

    % They are 2^256 - 1 and -(2^256 - 1), that is 64 Fs (note: not 2-complement, sign is not included)

    INT_MIN_16 = erlang:binary_to_integer(
        ?MODULE:id(<<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    <<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(
        INT_MIN_16, 16
    ),
    INT_MAX_16 = erlang:binary_to_integer(
        ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(
        INT_MAX_16, 16
    ),

    % They are 2^256 - 1 and -(2^256 - 1), that is 256 ones (note: not 2-complement, sign is not included)
    INT_MIN_2_BIN =
        <<"-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111">>,
    INT_MAX_2_BIN =
        <<"1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111">>,

    INT_MIN_2 = ?MODULE:id(erlang:binary_to_integer(?MODULE:id(INT_MIN_2_BIN), 2)),
    INT_MAX_2 = ?MODULE:id(erlang:binary_to_integer(?MODULE:id(INT_MAX_2_BIN), 2)),
    INT_MIN_2_BIN = ?MODULE:id(erlang:integer_to_binary(?MODULE:id(INT_MIN_2), 2)),
    INT_MAX_2_BIN = ?MODULE:id(erlang:integer_to_binary(?MODULE:id(INT_MAX_2), 2)),

    % Some random patterns

    Pattern1Bin = <<"-abcdeF123456789ABCDef98654311875421efcda91a2b3c4d5e6F7E6D5c4b3a7">>,
    Pattern1BinCanonical = <<"-ABCDEF123456789ABCDEF98654311875421EFCDA91A2B3C4D5E6F7E6D5C4B3A7">>,
    Pattern1Int = ?MODULE:id(erlang:binary_to_integer(?MODULE:id(Pattern1Bin), 16)),
    Pattern1BinCanonical = ?MODULE:id(integer_to_binary(?MODULE:id(Pattern1Int), 16)),

    Pattern2Bin =
        <<"000000000000000000000000000000000000000001010111010101101010011110101101011111001010110000100010111010101011110101010010110101010101010000001000101101000001000100010100101101001011111100101111101010101010001011101001010101110000101000111110000110110101001010101011111011100010101010101011011101011">>,
    Pattern2BinCanonical =
        <<"1010111010101101010011110101101011111001010110000100010111010101011110101010010110101010101010000001000101101000001000100010100101101001011111100101111101010101010001011101001010101110000101000111110000110110101001010101011111011100010101010101011011101011">>,
    Pattern2Int = erlang:binary_to_integer(?MODULE:id(Pattern2Bin), 2),
    Pattern2BinCanonical = integer_to_binary(Pattern2Int, 2),

    Pattern3Bin = <<"3ZE2L1OLJ3645OPTWC8GD2FQVJTR9PJJMA3Z9VEVFEML9L6IV5">>,
    Pattern3Int = ?MODULE:id(binary_to_integer(?MODULE:id(Pattern3Bin), 36)),
    Pattern3Bin = ?MODULE:id(integer_to_binary(?MODULE:id(Pattern3Int), 36)),

    Pattern4Bin = <<"-000000000000001bcdefghijklmnopqrstuvwxyza12345689ABCDEFJHIJKLMNZ">>,
    Pattern4BinCanonical = <<"-1BCDEFGHIJKLMNOPQRSTUVWXYZA12345689ABCDEFJHIJKLMNZ">>,
    Pattern4Int = ?MODULE:id(binary_to_integer(?MODULE:id(Pattern4Bin), 36)),
    Pattern4BinCanonical = ?MODULE:id(integer_to_binary(?MODULE:id(Pattern4Int), 36)),

    Pattern5Bin =
        <<"+000000000000BE636EFA1A9371DE7E57e4ecb7d9a921d792ab0b21b28c238C1F66AED27FB79F">>,
    Pattern5BinCanonical = <<"BE636EFA1A9371DE7E57E4ECB7D9A921D792AB0B21B28C238C1F66AED27FB79F">>,
    Pattern5Int = ?MODULE:id(binary_to_integer(?MODULE:id(Pattern5Bin), 16)),
    Pattern5BinCanonical = ?MODULE:id(integer_to_binary(?MODULE:id(Pattern5Int), 16)),

    Pattern6Bin =
        <<"-0000054826124455256601513636909251356536763516497895406989033472580562929119750424">>,
    Pattern6BinCanonical =
        <<"-54826124455256601513636909251356536763516497895406989033472580562929119750424">>,
    Pattern6Int = ?MODULE:id(binary_to_integer(?MODULE:id(Pattern6Bin), 10)),
    Pattern6BinCanonical = ?MODULE:id(integer_to_binary(?MODULE:id(Pattern6Int), 10)),

    Pattern7Bin =
        <<"-00000000000000000004534215062214255345551564500256544633040136644104631464312603650553545414012036651524002336">>,
    Pattern7BinCanonical =
        <<"-4534215062214255345551564500256544633040136644104631464312603650553545414012036651524002336">>,
    Pattern7Int = ?MODULE:id(binary_to_integer(?MODULE:id(Pattern7Bin), 7)),
    Pattern7BinCanonical = ?MODULE:id(integer_to_binary(?MODULE:id(Pattern7Int), 7)),

    ok = expect_error(badarg, fun() ->
        binary_to_integer(
            ?MODULE:id(
                <<"-45342150622142553455515645002565446330401366441046314643126036505535454140120366515240023z6">>
            ),
            7
        )
    end),

    TooBig1 = <<"10000000000000000000000000000000000000000000000000000000000000000">>,
    ok = expect_atomvm_error(badarg, fun() ->
        binary_to_integer(
            ?MODULE:id(
                TooBig1
            ),
            16
        )
    end),

    TooBig2 = <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    ok = expect_atomvm_error(badarg, fun() ->
        binary_to_integer(
            ?MODULE:id(
                TooBig2
            ),
            16
        )
    end),

    TooBig3 = <<"ACRLOAJ1MN6J7S7EH8796SS9GJF9GD34BPDF15DIES8ME9Q9G7HSG">>,
    ok = expect_atomvm_error(badarg, fun() ->
        binary_to_integer(
            ?MODULE:id(
                TooBig3
            ),
            29
        )
    end),

    TooBig4 = <<"2AVFFIPA2YC3I7N7GI96SUVLXY3W2PM5SW8JCGASD013YIUGHJ3MBVOYDJ9PIXSH0SNR4">>,
    ok = expect_atomvm_error(badarg, fun() ->
        binary_to_integer(
            ?MODULE:id(
                TooBig4
            ),
            35
        )
    end),

    TooBig5 =
        <<"2AVFFIPA2YC3I7N7GI96SUVLXY3W2PM5SW8JCGASD013YIUGHJ3MBVOYDJ9PIXSH0SNR40000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005">>,
    ok = expect_atomvm_error(badarg, fun() ->
        binary_to_integer(
            ?MODULE:id(
                TooBig5
            ),
            35
        )
    end),

    0.

test_integer_to_list() ->
    IntMaxBin = <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    IntMax = ?MODULE:id(erlang:binary_to_integer(?MODULE:id(IntMaxBin), 16)),
    "115792089237316195423570985008687907853269984665640564039457584007913129639935" = ?MODULE:id(
        erlang:integer_to_list(IntMax)
    ),
    "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" = ?MODULE:id(
        erlang:integer_to_list(IntMax, 16)
    ),

    IntMinBin = <<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    IntMin = ?MODULE:id(erlang:binary_to_integer(?MODULE:id(IntMinBin), 16)),
    "-115792089237316195423570985008687907853269984665640564039457584007913129639935" = ?MODULE:id(
        erlang:integer_to_list(IntMin)
    ),
    "-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" = ?MODULE:id(
        erlang:integer_to_list(IntMin, 16)
    ),

    RandBin = <<"313584127083402947713449759974837293576">>,
    RandInt = ?MODULE:id(erlang:binary_to_integer(?MODULE:id(RandBin))),
    "EBEA1B25A9CBB9DBC60F1D1FF7C19208" = ?MODULE:id(erlang:integer_to_list(RandInt, 16)),

    0.

test_integer_from_list() ->
    RandListDec = "1731841583231287768806110493630117706",
    RandIntDec = ?MODULE:id(erlang:list_to_integer(?MODULE:id(RandListDec))),
    <<"14D8A61E79E0FD73F68ED4EB6E9B74A">> = erlang:integer_to_binary(?MODULE:id(RandIntDec), 16),

    RandListHex = "97DD30E2C7C05611F18579A689C1A023",
    RandIntHex = ?MODULE:id(erlang:list_to_integer(?MODULE:id(RandListHex), 16)),
    <<"201861916492304234384630055011635798051">> = erlang:integer_to_binary(
        ?MODULE:id(RandIntHex), 10
    ),

    IntMaxList = "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
    IntMax = ?MODULE:id(erlang:list_to_integer(?MODULE:id(IntMaxList), 16)),
    <<"115792089237316195423570985008687907853269984665640564039457584007913129639935">> = erlang:integer_to_binary(
        ?MODULE:id(IntMax), 10
    ),

    PlusIntMaxList = "+FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
    PlusIntMax = ?MODULE:id(erlang:list_to_integer(?MODULE:id(PlusIntMaxList), 16)),
    <<"115792089237316195423570985008687907853269984665640564039457584007913129639935">> = erlang:integer_to_binary(
        ?MODULE:id(PlusIntMax), 10
    ),

    IntMinList = "-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
    IntMin = ?MODULE:id(erlang:list_to_integer(?MODULE:id(IntMinList), 16)),
    <<"-115792089237316195423570985008687907853269984665640564039457584007913129639935">> = erlang:integer_to_binary(
        ?MODULE:id(IntMin), 10
    ),

    Int0List = "8000000000000000",
    Int0 = ?MODULE:id(erlang:list_to_integer(?MODULE:id(Int0List), 16)),
    <<"9223372036854775808">> = erlang:integer_to_binary(?MODULE:id(Int0), 10),

    Int1List = "9223372036854775808",
    Int1 = ?MODULE:id(erlang:list_to_integer(?MODULE:id(Int1List), 10)),
    <<"9223372036854775808">> = erlang:integer_to_binary(?MODULE:id(Int1), 10),

    Int2List = "-8000000000000001",
    Int2 = ?MODULE:id(erlang:list_to_integer(?MODULE:id(Int2List), 16)),
    <<"-9223372036854775809">> = erlang:integer_to_binary(?MODULE:id(Int2), 10),

    Int3List = "-8000000000000001",
    Int3 = ?MODULE:id(erlang:list_to_integer(?MODULE:id(Int3List), 16)),
    <<"-9223372036854775809">> = erlang:integer_to_binary(?MODULE:id(Int3), 10),

    Int4List = "18446744073709551615",
    Int4 = ?MODULE:id(erlang:list_to_integer(?MODULE:id(Int4List))),
    <<"18446744073709551615">> = erlang:integer_to_binary(?MODULE:id(Int4)),

    Int5List = "18446744073709551616",
    Int5 = ?MODULE:id(erlang:list_to_integer(?MODULE:id(Int5List))),
    <<"18446744073709551616">> = erlang:integer_to_binary(?MODULE:id(Int5)),

    TooBig =
        "473G8HGH5SHXPHL0FW40LIZSMNW3BNJ51ABCT02HG4AKRJWXWI96A1W9UG2YQ9XNJ595OFX6ZUZWLNFZ2W1RYW49ZBUWZ16GXQE",
    ok = expect_atomvm_error(badarg, fun() ->
        list_to_integer(
            ?MODULE:id(
                TooBig
            ),
            36
        )
    end),

    0.

test_cmp() ->
    OutOfOrder = ?MODULE:the_out_of_order_list(),
    Ordered = ?MODULE:sort(OutOfOrder),
    true = (Ordered == binlist_to_integer(the_ordered_list())),
    EndianessOutOfOrder = [
        0,
        erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFFFFFFFFFFBBBBBBBBEEEEEEEE">>), 16),
        erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFFFFFFFFFF00000000FFFFFFFF">>), 16),
        erlang:binary_to_integer(?MODULE:id(<<"BBBBBBBBEEEEEEEEFFFFFFFFFFFFFFFF">>), 16),
        erlang:binary_to_integer(?MODULE:id(<<"00000000FFFFFFFFFFFFFFFFFFFFFFFF">>), 16)
    ],
    EndianessOrdered = [
        0,
        erlang:binary_to_integer(?MODULE:id(<<"00000000FFFFFFFFFFFFFFFFFFFFFFFF">>), 16),
        erlang:binary_to_integer(?MODULE:id(<<"BBBBBBBBEEEEEEEEFFFFFFFFFFFFFFFF">>), 16),
        erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFFFFFFFFFF00000000FFFFFFFF">>), 16),
        erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFFFFFFFFFFBBBBBBBBEEEEEEEE">>), 16)
    ],
    EndianessOrdered = ?MODULE:sort(EndianessOutOfOrder),
    0.

binlist_to_integer([]) ->
    [];
binlist_to_integer([H | T]) ->
    [erlang:binary_to_integer(H) | binlist_to_integer(T)].

the_out_of_order_list() ->
    [
        ?MODULE:id(
            erlang:binary_to_integer(
                ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>),
                16
            )
        ),
        ?MODULE:id(
            erlang:binary_to_integer(
                ?MODULE:id(<<"1BCDEFGHIJKLMNOPQRSTUVWXYZA12345689ABCDEFJHIJKLMNZ">>), 36
            )
        ),
        ?MODULE:id(
            erlang:binary_to_integer(
                ?MODULE:id(<<"-1BCDEFGHIJKLMNOPQRSTUVWXYZA12345689ABCDEFJHIJKLMNZ">>), 36
            )
        ),
        10,
        -23,
        ?MODULE:pow(-2, 39),
        ?MODULE:pow(2, 63),
        9,
        ?MODULE:pow(2, 39),
        0,
        ?MODULE:id(
            erlang:binary_to_integer(
                ?MODULE:id(<<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>),
                16
            )
        ),
        0,
        ?MODULE:pow(2, 40),
        0,
        ?MODULE:pow(-2, 31),
        ?MODULE:id(
            erlang:binary_to_integer(
                ?MODULE:id(<<"-1BCDEFGHIJKLMNOPQRSTUVWXYZA12345689ABCDEFJHIJKLMNZ">>), 36
            )
        ),
        0,
        -1,
        1,
        5,
        ?MODULE:pow(2, 31),
        ?MODULE:pow(-2, 47),
        ?MODULE:pow(-2, 63),
        89,
        -1,
        0,
        0,
        1,
        ?MODULE:id(
            erlang:binary_to_integer(
                ?MODULE:id(<<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>),
                16
            )
        ),
        ?MODULE:id(
            erlang:binary_to_integer(
                ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE">>),
                16
            )
        ),
        ?MODULE:id(
            erlang:binary_to_integer(
                ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD">>),
                16
            )
        ),
        ?MODULE:id(
            erlang:binary_to_integer(
                ?MODULE:id(<<"EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>),
                16
            )
        ),
        ?MODULE:id(
            erlang:binary_to_integer(
                ?MODULE:id(<<"DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>),
                16
            )
        ),
        ?MODULE:id(
            erlang:binary_to_integer(
                ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>),
                16
            )
        ),
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"-FFFFFFFFFFFFFFFF">>), 16)),
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"-FFFFFFFFFFFFFFFE">>), 16)),
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"-10000000000000000">>), 16)),
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFFFFFFFFFF">>), 16)),
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"10000000000000000">>), 16)),
        0,
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFF">>), 16)),
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFE">>), 16)),
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"-FFFFFFFF">>), 16)),
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"-FFFFFFFE">>), 16)),
        2,
        3,
        0,
        -20,
        20,
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"100000000">>), 16)),
        -1,
        -2,
        -3,
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"-100000000">>), 16)),
        16#FFFFFFF,
        16#FFFFFFE,
        -16#FFFFFFF,
        -16#FFFFFFE,
        16#10000000,
        -16#10000000,
        16#10000001,
        -16#10000001,
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFFFFFFFFF">>), 16)),
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFFFFFFFFE">>), 16)),
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"-FFFFFFFFFFFFFFF">>), 16)),
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"-FFFFFFFFFFFFFFE">>), 16)),
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"FFFFFFFFFFFFFFF">>), 16)),
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"1000000000000000">>), 16)),
        ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"-1000000000000000">>), 16)),
        ?MODULE:fact(47),
        ?MODULE:fact(48),
        ?MODULE:fact(49),
        ?MODULE:id(
            erlang:binary_to_integer(
                ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>),
                16
            )
        ),
        ?MODULE:id(
            erlang:binary_to_integer(
                ?MODULE:id(<<"1BCDEFGHIJKLMNOPQRSTUVWXYZA12345689ABCDEFJHIJKLMNZ">>), 36
            )
        ),
        0,
        -89,
        94,
        -94,
        81,
        -81,
        ?MODULE:id(
            erlang:binary_to_integer(
                ?MODULE:id(<<"EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>),
                16
            )
        ),
        ?MODULE:id(
            erlang:binary_to_integer(
                ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFF2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>),
                16
            )
        ),
        ?MODULE:pow(2, 64),
        ?MODULE:pow(2, 63)
    ].

the_ordered_list() ->
    [
        <<"-115792089237316195423570985008687907853269984665640564039457584007913129639935">>,
        <<"-115792089237316195423570985008687907853269984665640564039457584007913129639935">>,
        <<"-23866129307451569834960726085978030586952270370050797044683392240429208077823">>,
        <<"-23866129307451569834960726085978030586952270370050797044683392240429208077823">>,
        <<"-18446744073709551616">>,
        <<"-18446744073709551615">>,
        <<"-18446744073709551614">>,
        <<"-9223372036854775808">>,
        <<"-1152921504606846976">>,
        <<"-1152921504606846975">>,
        <<"-1152921504606846974">>,
        <<"-140737488355328">>,
        <<"-549755813888">>,
        <<"-4294967296">>,
        <<"-4294967295">>,
        <<"-4294967294">>,
        <<"-2147483648">>,
        <<"-268435457">>,
        <<"-268435456">>,
        <<"-268435455">>,
        <<"-268435454">>,
        <<"-94">>,
        <<"-89">>,
        <<"-81">>,
        <<"-23">>,
        <<"-20">>,
        <<"-3">>,
        <<"-2">>,
        <<"-1">>,
        <<"-1">>,
        <<"-1">>,
        <<"0">>,
        <<"0">>,
        <<"0">>,
        <<"0">>,
        <<"0">>,
        <<"0">>,
        <<"0">>,
        <<"0">>,
        <<"0">>,
        <<"1">>,
        <<"1">>,
        <<"2">>,
        <<"3">>,
        <<"5">>,
        <<"9">>,
        <<"10">>,
        <<"20">>,
        <<"81">>,
        <<"89">>,
        <<"94">>,
        <<"268435454">>,
        <<"268435455">>,
        <<"268435456">>,
        <<"268435457">>,
        <<"2147483648">>,
        <<"4294967294">>,
        <<"4294967295">>,
        <<"4294967296">>,
        <<"549755813888">>,
        <<"1099511627776">>,
        <<"1152921504606846974">>,
        <<"1152921504606846975">>,
        <<"1152921504606846975">>,
        <<"1152921504606846976">>,
        <<"9223372036854775808">>,
        <<"9223372036854775808">>,
        <<"18446744073709551615">>,
        <<"18446744073709551616">>,
        <<"18446744073709551616">>,
        <<"258623241511168180642964355153611979969197632389120000000000">>,
        <<"12413915592536072670862289047373375038521486354677760000000000">>,
        <<"608281864034267560872252163321295376887552831379210240000000000">>,
        <<"23866129307451569834960726085978030586952270370050797044683392240429208077823">>,
        <<"23866129307451569834960726085978030586952270370050797044683392240429208077823">>,
        <<"101318078082651670995624611882601919371611236582435493534525386006923988434943">>,
        <<"108555083659983933209597798445644913612440610624038028786991485007418559037439">>,
        <<"108555083659983933209597798445644913612440610624038028786991485007418559037439">>,
        <<"115792089237316195423570984985303881655975537974381606715997055693418208952319">>,
        <<"115792089237316195423570985008687617943582403767539724075120039579213551894527">>,
        <<"115792089237316195423570985008687907853269984665640564039457584007913129639933">>,
        <<"115792089237316195423570985008687907853269984665640564039457584007913129639934">>,
        <<"115792089237316195423570985008687907853269984665640564039457584007913129639935">>,
        <<"115792089237316195423570985008687907853269984665640564039457584007913129639935">>
    ].

sort([Pivot | T]) ->
    sort([X || X <- T, X < Pivot]) ++
        [Pivot] ++
        sort([X || X <- T, X >= Pivot]);
sort([]) ->
    [].

conv_to_from_float() ->
    % to float

    Int0 = ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"1000000000000000000">>), 16)),
    Int1 = ?MODULE:id(
        erlang:binary_to_integer(?MODULE:id(<<"CAFECAFE1234000000000000000000">>), 16)
    ),
    Int2 = ?MODULE:id(
        erlang:binary_to_integer(?MODULE:id(<<"-CAFECAFE1234000000000000000000">>), 16)
    ),
    Int3 = ?MODULE:mul(?MODULE:id(Int1), 2),
    Num1 = ?MODULE:mul(?MODULE:id(Int1), ?MODULE:id(erlang:binary_to_float(?MODULE:id(<<"1.0">>)))),
    Num2 = ?MODULE:mul(?MODULE:id(Int2), ?MODULE:id(erlang:binary_to_float(?MODULE:id(<<"1.0">>)))),
    Num3 = ?MODULE:id(Int1) * ?MODULE:id(erlang:binary_to_float(?MODULE:id(<<"2.0">>))),
    true =
        erlang:binary_to_integer(?MODULE:id(<<"CAFECAFE1234">>), 16) =:=
            ?MODULE:divtrunc(?MODULE:id(Num1), Int0),
    true =
        erlang:binary_to_integer(?MODULE:id(<<"-CAFECAFE1234">>), 16) =:=
            ?MODULE:divtrunc(?MODULE:id(Num2), Int0),
    true =
        erlang:binary_to_integer(?MODULE:id(<<"195FD95FC2468">>), 16) =:=
            ?MODULE:divtrunc(?MODULE:id(Num3), Int0),

    % from float

    Int1 = ?MODULE:id(trunc(?MODULE:id(Num1))),
    Int2 = ?MODULE:id(round(?MODULE:id(Num2))),
    Int3 = ?MODULE:id(floor(?MODULE:id(Num3))),
    Int3 = ?MODULE:id(ceil(?MODULE:id(Num3))),

    Int64Max = ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"7FFFFFFFFFFFFFFF">>), 16)),
    true = (Int64Max >= ?MODULE:id(trunc(?MODULE:id(9223372036854775295.0)))),
    true = (Int64Max < ?MODULE:id(trunc(?MODULE:id(9223372036854775296.0)))),

    Int64Min = ?MODULE:id(erlang:binary_to_integer(?MODULE:id(<<"-8000000000000000">>), 16)),
    true = (Int64Min =< ?MODULE:id(trunc(?MODULE:id(-9223372036854776832.0)))),
    true = (Int64Min > ?MODULE:id(trunc(?MODULE:id(-9223372036854776833.0)))),

    % test limits and comparisons
    MaxInt = erlang:binary_to_integer(
        ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    MaxIntAsFloat = erlang:float(?MODULE:id(MaxInt)),
    true = (?MODULE:id(1.111111111111111e77) < MaxIntAsFloat),
    true = (MaxIntAsFloat < ?MODULE:id(1.888888888888888e77)),

    MinInt = erlang:binary_to_integer(
        ?MODULE:id(<<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    MinIntAsFloat = erlang:float(?MODULE:id(MinInt)),
    true = (?MODULE:id(-1.111111111111111e77) > MinIntAsFloat),
    true = (MinIntAsFloat > ?MODULE:id(-1.888888888888888e77)),

    % test overflows
    ok = expect_overflow(fun() -> trunc(?MODULE:id(1.157920892373163e77)) end),
    ok = expect_overflow(fun() -> trunc(?MODULE:id(-1.157920892373163e77)) end),

    true = (trunc(?MODULE:id(1.157920892373160e77)) > ?MODULE:pow(2, 255)),
    true = (trunc(?MODULE:id(-1.157920892373160e77)) < ?MODULE:pow(-2, 255)),

    0.

divtrunc(X, Y) ->
    erlang:trunc(X / Y).

external_term_decode() ->
    T1B = ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9E024D5C1207BCB8FCDD50C17BBBB">>),
    T1 = ?MODULE:id(erlang:binary_to_integer(T1B, 16)),
    T1 = ?MODULE:id(
        erlang:binary_to_term(
            ?MODULE:id(
                <<131, 110, 32, 0, 187, 187, 23, 12, 213, 205, 143, 203, 123, 32, 193, 213, 36, 224,
                    249, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                    255, 255>>
            )
        )
    ),
    T2B = ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9E024D5C1207BCB8FCDD50C17BDA">>),
    T2 = ?MODULE:id(erlang:binary_to_integer(T2B, 16)),
    T2 = ?MODULE:id(
        erlang:binary_to_term(
            ?MODULE:id(
                <<131, 110, 32, 0, 218, 123, 193, 80, 221, 252, 184, 188, 7, 18, 92, 77, 2, 158,
                    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                    255, 15>>
            )
        )
    ),
    T3B = ?MODULE:id(<<"-FFFFFFFFFFFFFFFF">>),
    T3 = ?MODULE:id(erlang:binary_to_integer(T3B, 16)),
    T3 = ?MODULE:id(
        erlang:binary_to_term(
            ?MODULE:id(<<131, 110, 8, 1, 255, 255, 255, 255, 255, 255, 255, 255>>)
        )
    ),

    % 16#10000000000000000000000000000000000000000000000000000000000000000 = 2^256
    TooBig1 =
        <<131, 110, 33, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 1>>,
    ok = expect_atomvm_error(
        badarg,
        fun() ->
            erlang:binary_to_term(
                ?MODULE:id(TooBig1)
            )
        end
    ),

    % {foo, #{16#10000000000000000000000000000000000000000000000000000000000000000 => <<"bar">>}}
    TooBig2 =
        <<131, 104, 2, 119, 3, 102, 111, 111, 116, 0, 0, 0, 1, 110, 33, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 109, 0, 0, 0,
            3, 98, 97, 114>>,
    ok = expect_atomvm_error(
        badarg,
        fun() ->
            erlang:binary_to_term(
                ?MODULE:id(TooBig2)
            )
        end
    ),

    % 16#1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 = 2^600
    TooBig3 =
        <<131, 110, 76, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1>>,
    ok = expect_atomvm_error(
        badarg,
        fun() ->
            erlang:binary_to_term(
                ?MODULE:id(TooBig3)
            )
        end
    ),

    0.

test_big_literals() ->
    % Note: big literals might be encoded in 2 different ways:
    % - Inside the code stream, using an nbits compact term encoding
    % - As literals in the literals table
    % Big integers above a certain size are stored in the literals table.
    % This function will test both encodings.

    <<"-CAFE1234ABCD9876EFAB0189FEDCBA98">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#CAFE1234ABCD9876EFAB0189FEDCBA98), 16)
    ),
    <<"-CAFE1234ABCD9876EFAB0189FEDCBA984">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#CAFE1234ABCD9876EFAB0189FEDCBA984), 16)
    ),
    <<"-CAFE1234ABCD9876EFAB0189FEDCBA9842">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#CAFE1234ABCD9876EFAB0189FEDCBA9842), 16)
    ),
    <<"CAFE1234ABCD9876EFAB0189FEDCBA9842">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#CAFE1234ABCD9876EFAB0189FEDCBA9842), 16)
    ),

    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF), 16
        )
    ),

    <<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF), 16
        )
    ),

    % These cannot be tested (yet)
    % bigger literals, such as the ones here below, are encoded using an external term
    % (having SMALL_BIG_EXT type) inside the literal table.
    % The reader function is not able to distinguish between different kind of errors,
    % such as overflow, so this cannot be tested yet.
    % ok = expect_overflow(fun ?MODULE:lit_ovf1/0),
    % ok = expect_overflow(fun ?MODULE:lit_ovf2/0),

    % Integers close to the INT64_MIN / INT64_MAX / (+-)UINT64_MAX boundary
    <<"8000000000000000">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#8000000000000000), 16)
    ),
    <<"8000000000000001">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#8000000000000001), 16)
    ),
    <<"-8000000000000001">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#8000000000000001), 16)
    ),
    <<"FFFFFFFFFFFFFFFF">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#FFFFFFFFFFFFFFFF), 16)
    ),
    <<"-FFFFFFFFFFFFFFFF">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#FFFFFFFFFFFFFFFF), 16)
    ),

    % Random n-bits positive integers

    % 64 bits
    <<"5AE3C2DF4EBB8E47">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#5AE3C2DF4EBB8E47), 16)
    ),
    % 70 bits
    <<"BD63E16950B2629E6">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#BD63E16950B2629E6), 16)
    ),
    % 72 bits
    <<"94BC3A736478DBA666">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#94BC3A736478DBA666), 16)
    ),
    % 77 bits
    <<"11AA908CAA57C6AC0D35">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#11AA908CAA57C6AC0D35), 16)
    ),
    % 80 bits
    <<"44F043DBDE24FE653434">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#44F043DBDE24FE653434), 16)
    ),
    % 80 bits
    <<"5EB9B1EC9951E212A487">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#5EB9B1EC9951E212A487), 16)
    ),
    % 84 bits
    <<"ECDF4565C3B7DEC2FB494">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#ECDF4565C3B7DEC2FB494), 16)
    ),
    % 88 bits
    <<"1053B376041F92B5726341">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#1053B376041F92B5726341), 16)
    ),
    % 91 bits
    <<"57732E06A8E4A31A06F9063">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#57732E06A8E4A31A06F9063), 16)
    ),
    % 96 bits
    <<"4866A8DB2B29CFAC068E9ECB">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#4866A8DB2B29CFAC068E9ECB), 16)
    ),
    % 96 bits
    <<"AEF4EA41F77AF3767522B152">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#AEF4EA41F77AF3767522B152), 16)
    ),
    % 98 bits
    <<"37C44D15824939B568A993235">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#37C44D15824939B568A993235), 16)
    ),
    % 104 bits
    <<"B025687FEF2AFB2523D0F108DF">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#B025687FEF2AFB2523D0F108DF), 16)
    ),
    % 112 bits
    <<"BC425FB3F10FD6792E11342466F9">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#BC425FB3F10FD6792E11342466F9), 16)
    ),
    % 119 bits
    <<"14C20B9BBDE1DFC9F12F3E1728BD69">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#14C20B9BBDE1DFC9F12F3E1728BD69), 16)
    ),
    % 133 bits
    <<"13828C529D8120F031B42006C90D7ADCA7">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#13828C529D8120F031B42006C90D7ADCA7), 16)
    ),
    % 168 bits
    <<"AA031DB9E5BB7A2495374BA85FF072CFC3EEA03C26">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#AA031DB9E5BB7A2495374BA85FF072CFC3EEA03C26), 16)
    ),
    % 175 bits
    <<"215D2DF589F57C5B14C75EE254D62DD616AE6DAFC3B9">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#215D2DF589F57C5B14C75EE254D62DD616AE6DAFC3B9), 16)
    ),
    % 184 bits
    <<"8DF74EFF758D9FB749A6A6CA82E283972C92CDF467129F">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(16#8DF74EFF758D9FB749A6A6CA82E283972C92CDF467129F), 16)
    ),
    % 189 bits
    <<"19962BED9F6051C550C8EC823426B68BAD3C4B8B70A3787B">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(16#19962BED9F6051C550C8EC823426B68BAD3C4B8B70A3787B), 16
        )
    ),
    % 192 bits
    <<"9B56F43583ED7B4F6DD67E27FB1C3961A1BD29F448397A5A">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(16#9B56F43583ED7B4F6DD67E27FB1C3961A1BD29F448397A5A), 16
        )
    ),
    % 200 bits
    <<"84A8A7E337383233C9AEF55265435F9AA1B8567E5D0EA101A">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(16#84A8A7E337383233C9AEF55265435F9AA1B8567E5D0EA101A), 16
        )
    ),
    % 208 bits
    <<"BB1346A60EE8CBCF889A4BFC465E96D55B7FB4114FA84376B23D">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(16#BB1346A60EE8CBCF889A4BFC465E96D55B7FB4114FA84376B23D), 16
        )
    ),
    % 217 bits
    <<"85824F4804BF631786F146BBDC36482A681B0E62EE182D8678EB4D">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(16#85824F4804BF631786F146BBDC36482A681B0E62EE182D8678EB4D), 16
        )
    ),
    % 224 bits
    <<"EF554FF3797C57396FF919966063F47F0217D79CC3A1A2DB9966E9A9">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(16#EF554FF3797C57396FF919966063F47F0217D79CC3A1A2DB9966E9A9), 16
        )
    ),
    % 231 bits
    <<"228271FD505D1AE685D9558BD1D916C5F0DB612F8A5E515A4CC610195F">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(16#228271FD505D1AE685D9558BD1D916C5F0DB612F8A5E515A4CC610195F), 16
        )
    ),
    % 232 bits
    <<"DAF21384A796A58CEDE99525AF336ECBCC7AC01C4AD4592E902EE4A046">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(16#DAF21384A796A58CEDE99525AF336ECBCC7AC01C4AD4592E902EE4A046), 16
        )
    ),
    % 248 bits
    <<"1D9D32C0A97D74D75266096C9D43DD9C108B060B0B33054A47ACB70F9C7082">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(16#1D9D32C0A97D74D75266096C9D43DD9C108B060B0B33054A47ACB70F9C7082), 16
        )
    ),
    % 256 bits
    <<"9A988604ED17067CFE04BBB5B1B96958D66F1E910B4C7C008DA2A9D56605F630">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(16#9A988604ED17067CFE04BBB5B1B96958D66F1E910B4C7C008DA2A9D56605F630), 16
        )
    ),

    % Random n-bits negative integers

    % 64 bits
    <<"-50818490C479D1F">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#50818490C479D1F), 16)
    ),
    % 72 bits
    <<"-7AB5EF1509FB36264D">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#7AB5EF1509FB36264D), 16)
    ),
    % 80 bits
    <<"-A1425ECB24D0B8FB90F2">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#A1425ECB24D0B8FB90F2), 16)
    ),
    % 81 bits
    <<"-77DF44280198DC720D93">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#77DF44280198DC720D93), 16)
    ),
    % 88 bits
    <<"-362BF6716A0443139EC23A">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#362BF6716A0443139EC23A), 16)
    ),
    % 90 bits
    <<"-147127EA7E81F288D1195EA">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#147127EA7E81F288D1195EA), 16)
    ),
    % 96 bits
    <<"-6221559B9B8B13F17C279383">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#6221559B9B8B13F17C279383), 16)
    ),
    % 99 bits
    <<"-319DEFD10B261A2C660209444">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#319DEFD10B261A2C660209444), 16)
    ),
    % 104 bits
    <<"-1B144E948BEC42B88641A831A5">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#1B144E948BEC42B88641A831A5), 16)
    ),
    % 108 bits
    <<"-4C63771CFE84846685BC0396B35">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#4C63771CFE84846685BC0396B35), 16)
    ),
    % 112 bits
    <<"-F28ADD446F3A789187A438A40CCD">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#F28ADD446F3A789187A438A40CCD), 16)
    ),
    % 117 bits
    <<"-2EDF2F4B2D52F77795D2B2BAD61C6">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#2EDF2F4B2D52F77795D2B2BAD61C6), 16)
    ),
    % 120 bits
    <<"-EE1D7598927B405203453B5DA02DE3">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#EE1D7598927B405203453B5DA02DE3), 16)
    ),
    % 126 bits
    <<"-168688DF9FBC275817A0855D76A45132">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#168688DF9FBC275817A0855D76A45132), 16)
    ),
    % 128 bits
    <<"-AE485055D76AB0A72DD218C3125FBF8C">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#AE485055D76AB0A72DD218C3125FBF8C), 16)
    ),
    % 135 bits
    <<"-7E440D81FA0C22C1E93FD5983673C647F2">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#7E440D81FA0C22C1E93FD5983673C647F2), 16)
    ),
    % 136 bits
    <<"-ED98CC050A2296AF338C3687DA3ACF420F">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#ED98CC050A2296AF338C3687DA3ACF420F), 16)
    ),
    % 144 bits
    <<"-3A1A12A0A958D70DF98188DB04615FC1F20B">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#3A1A12A0A958D70DF98188DB04615FC1F20B), 16)
    ),
    % 152 bits
    <<"-33BADCFC1628C22A56CE3DD3BDE2FF1AF1362B">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#33BADCFC1628C22A56CE3DD3BDE2FF1AF1362B), 16)
    ),
    % 153 bits
    <<"-1B83FB643871FFCEBEDEA6DA1D400AB825D2BAE">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#1B83FB643871FFCEBEDEA6DA1D400AB825D2BAE), 16)
    ),
    % 160 bits
    <<"-86DCDAA30A967D5015E278476420B03FD735601C">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#86DCDAA30A967D5015E278476420B03FD735601C), 16)
    ),
    % 162 bits
    <<"-2796D95EBA7CB2CF780A9445A21BF1FE8CDD0C424">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#2796D95EBA7CB2CF780A9445A21BF1FE8CDD0C424), 16)
    ),
    % 168 bits
    <<"-7AD7CD85DE09B44F69940A3EFE46E762D6E3851140">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#7AD7CD85DE09B44F69940A3EFE46E762D6E3851140), 16)
    ),
    % 171 bits
    <<"-2D72B15CED61DBBB23B49622CCA465257744D7909D0">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#2D72B15CED61DBBB23B49622CCA465257744D7909D0), 16)
    ),
    % 176 bits
    <<"-3EBF0A61C92427B2A38ABE310B995CE3904BB021BCEB">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#3EBF0A61C92427B2A38ABE310B995CE3904BB021BCEB), 16)
    ),
    % 180 bits
    <<"-D96EBE3BBA68169F689ACAEAA430BD45961DA58FCBDBC">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#D96EBE3BBA68169F689ACAEAA430BD45961DA58FCBDBC), 16)
    ),
    % 184 bits
    <<"-27D044D547D4823D85598B77B1DC00C6F0F2B630318B50">> = ?MODULE:id(
        erlang:integer_to_binary(?MODULE:id(-16#27D044D547D4823D85598B77B1DC00C6F0F2B630318B50), 16)
    ),
    % 189 bits
    <<"-6839607C30B5339F8C9E651811FF69313D2946B27E0CEF7">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#6839607C30B5339F8C9E651811FF69313D2946B27E0CEF7), 16
        )
    ),
    % 192 bits
    <<"-241A871C25CB5CE38DD33B007C1A062CB87571F0F69A4A1C">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#241A871C25CB5CE38DD33B007C1A062CB87571F0F69A4A1C), 16
        )
    ),
    % 198 bits
    <<"-D6F7F1831498E593F970E7CEB7FA5140002D4B6C15B87B05A">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#D6F7F1831498E593F970E7CEB7FA5140002D4B6C15B87B05A), 16
        )
    ),
    % 200 bits
    <<"-C354EBE297757034B878D06A760C200FEDB44A54A294044B34">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#C354EBE297757034B878D06A760C200FEDB44A54A294044B34), 16
        )
    ),
    % 207 bits
    <<"-35F847B9EDE1F95AA0BD703EEC13B957A514EF7C9F9945C2BD2A">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#35F847B9EDE1F95AA0BD703EEC13B957A514EF7C9F9945C2BD2A), 16
        )
    ),
    % 208 bits
    <<"-80D50449943E584056D6C6132C6C9ECA4FCC17B683DD0D65B983">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#80D50449943E584056D6C6132C6C9ECA4FCC17B683DD0D65B983), 16
        )
    ),
    % 216 bits
    <<"-7E187419A7274C12DCD5414DC35EB750B6BEC9448EBFE52E235CE5">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#7E187419A7274C12DCD5414DC35EB750B6BEC9448EBFE52E235CE5), 16
        )
    ),
    % 224 bits
    <<"-A364C94BC4F37B2F61E79C605E7E105EB6DD9031B467289FDAA728A8">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#A364C94BC4F37B2F61E79C605E7E105EB6DD9031B467289FDAA728A8), 16
        )
    ),
    % 225 bits
    <<"-16D3D742124EF7F62F3A7AA59B5DEA065FCE1A4BD9121D3FD277A713C">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#16D3D742124EF7F62F3A7AA59B5DEA065FCE1A4BD9121D3FD277A713C), 16
        )
    ),
    % 232 bits
    <<"-858C6BD55C8ABE9EAD2A0BDED5CCB2B48C19257DE8576223992E63A5C2">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#858C6BD55C8ABE9EAD2A0BDED5CCB2B48C19257DE8576223992E63A5C2), 16
        )
    ),
    % 234 bits
    <<"-2CB07CC22D0ACDC310636D462BDA0510BC46078732929DAB10C35212BAC">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#2CB07CC22D0ACDC310636D462BDA0510BC46078732929DAB10C35212BAC), 16
        )
    ),
    % 240 bits
    <<"-7AB9F21FF113FD7F3B00DEBF3038BF66F573CC99B7B9042303C491B0C6EC">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#7AB9F21FF113FD7F3B00DEBF3038BF66F573CC99B7B9042303C491B0C6EC), 16
        )
    ),
    % 243 bits
    <<"-73986874A2F0F72F85E956DDC5933EB1E5A5380BE618FD93C07838DD9C543">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#73986874A2F0F72F85E956DDC5933EB1E5A5380BE618FD93C07838DD9C543), 16
        )
    ),
    % 248 bits
    <<"-4C62CACE1C39245B3571CDB3C36EA34C54B89542AC5D479B832396D74E3251">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#4C62CACE1C39245B3571CDB3C36EA34C54B89542AC5D479B832396D74E3251), 16
        )
    ),
    % 252 bits
    <<"-BC9820F853B21F8B1AAEF6BB7ECBC67FF9497B98E4846C3F7097259483623DE">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#BC9820F853B21F8B1AAEF6BB7ECBC67FF9497B98E4846C3F7097259483623DE), 16
        )
    ),
    % 256 bits
    <<"-AE0FA6658F605BD9D70AC5AB29BD4164992CE77586E39BFEA4F04E5D417D1E0B">> = ?MODULE:id(
        erlang:integer_to_binary(
            ?MODULE:id(-16#AE0FA6658F605BD9D70AC5AB29BD4164992CE77586E39BFEA4F04E5D417D1E0B), 16
        )
    ),

    11778076840785789394209099624956350279955 = list_sum(
        ?MODULE:id(
            [
                281532703474492501731626716290297310019,
                93306314956976059883019272415387330003,
                214607399141421462250690668733314471655,
                278088529617771354220434498525634157025,
                334122505095354375669507689190891893146,
                97282639123841533893464348356498690111,
                139101277621645745787465467869309404885,
                336308780620891755582747031067683052383,
                304507291449018005568388967257032414400,
                89470201992235757207956459849942467761
            ]
        ),
        1,
        0
    ),

    0.

% This function will never be called, we leave this to check if we are able to parse the BEAM file
% even if this integer exceeds maximum integer capacity.
lit_ovf1() ->
    ?MODULE:id(16#10000000000000000000000000000000000000000000000000000000000000000).

% This function will never be called, we leave this to check if we are able to parse the BEAM file
% even if this integer exceeds maximum integer capacity.
lit_ovf2() ->
    ?MODULE:id(-16#10000000000000000000000000000000000000000000000000000000000000000).

list_sum([], _Index, Acc) ->
    Acc;
list_sum([N | Tail], Index, Acc) ->
    list_sum(Tail, Index + 1, N * Index + Acc).

test_is_integer() ->
    MaxPatternBin = <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    MaxPattern = erlang:binary_to_integer(?MODULE:id(MaxPatternBin), 16),
    ok = ?MODULE:is_integer_helper(?MODULE:id(MaxPattern)),
    true = is_integer(?MODULE:id(MaxPattern)),

    MinPatternBin = <<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    MinPattern = erlang:binary_to_integer(?MODULE:id(MinPatternBin), 16),
    ok = ?MODULE:is_integer_helper(?MODULE:id(MinPattern)),
    true = is_integer(?MODULE:id(MinPattern)),

    Pattern128Bin = <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    Pattern128 = erlang:binary_to_integer(?MODULE:id(Pattern128Bin), 16),
    ok = ?MODULE:is_integer_helper(?MODULE:id(Pattern128)),
    true = is_integer(?MODULE:id(Pattern128)),

    Pattern128NegBin = <<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    Pattern128Neg = erlang:binary_to_integer(?MODULE:id(Pattern128NegBin), 16),
    ok = ?MODULE:is_integer_helper(?MODULE:id(Pattern128Neg)),
    true = is_number(?MODULE:id(Pattern128Neg)),

    UINT64MaxBin = <<"FFFFFFFFFFFFFFFF">>,
    UINT64Max = erlang:binary_to_integer(?MODULE:id(UINT64MaxBin), 16),
    ok = ?MODULE:is_integer_helper(?MODULE:id(UINT64Max)),
    true = is_number(?MODULE:id(UINT64Max)),

    UINT64MaxNegBin = <<"-FFFFFFFFFFFFFFFF">>,
    UINT64MaxNeg = erlang:binary_to_integer(?MODULE:id(UINT64MaxNegBin), 16),
    ok = ?MODULE:is_integer_helper(?MODULE:id(UINT64MaxNeg)),
    true = is_number(?MODULE:id(UINT64MaxNeg)),

    INT63MaxP1Bin = <<"8000000000000000">>,
    INT63MaxP1 = erlang:binary_to_integer(?MODULE:id(INT63MaxP1Bin), 16),
    ok = ?MODULE:is_integer_helper(?MODULE:id(INT63MaxP1)),
    true = is_number(?MODULE:id(INT63MaxP1)),

    INT63MinM1Bin = <<"-8000000000000001">>,
    INT63MinM1 = erlang:binary_to_integer(?MODULE:id(INT63MinM1Bin), 16),
    ok = ?MODULE:is_integer_helper(?MODULE:id(INT63MinM1)),
    true = is_number(?MODULE:id(INT63MinM1)),

    MaxPatternBin = <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    MaxPattern = erlang:binary_to_integer(?MODULE:id(MaxPatternBin), 16),
    ok = ?MODULE:is_integer_helper(?MODULE:id(MaxPattern)),
    true = is_number(?MODULE:id(MaxPattern)),

    RandomPatternBin = <<"4LWS1KF502AD5JUXQCS">>,
    RandomPattern = erlang:binary_to_integer(?MODULE:id(RandomPatternBin), 35),
    ok = ?MODULE:is_integer_helper(?MODULE:id(RandomPattern)),
    true = is_number(?MODULE:id(RandomPattern)),

    0.

is_integer_helper(I) when is_integer(I) ->
    _ = ?MODULE:id(I),
    ok;
is_integer_helper(_I) ->
    ?MODULE:id(error).

test_is_number() ->
    MaxPatternBin = <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    MaxPattern = erlang:binary_to_integer(?MODULE:id(MaxPatternBin), 16),
    ok = ?MODULE:is_number_helper(?MODULE:id(MaxPattern)),
    true = is_number(?MODULE:id(MaxPattern)),

    MinPatternBin = <<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    MinPattern = erlang:binary_to_integer(?MODULE:id(MinPatternBin), 16),
    ok = ?MODULE:is_number_helper(?MODULE:id(MinPattern)),
    true = is_number(?MODULE:id(MinPattern)),

    Pattern128Bin = <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    Pattern128 = erlang:binary_to_integer(?MODULE:id(Pattern128Bin), 16),
    ok = ?MODULE:is_number_helper(?MODULE:id(Pattern128)),
    true = is_number(?MODULE:id(Pattern128)),

    Pattern128NegBin = <<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    Pattern128Neg = erlang:binary_to_integer(?MODULE:id(Pattern128NegBin), 16),
    ok = ?MODULE:is_number_helper(?MODULE:id(Pattern128Neg)),
    true = is_number(?MODULE:id(Pattern128Neg)),

    UINT64MaxBin = <<"FFFFFFFFFFFFFFFF">>,
    UINT64Max = erlang:binary_to_integer(?MODULE:id(UINT64MaxBin), 16),
    ok = ?MODULE:is_number_helper(?MODULE:id(UINT64Max)),
    true = is_number(?MODULE:id(UINT64Max)),

    UINT64MaxNegBin = <<"-FFFFFFFFFFFFFFFF">>,
    UINT64MaxNeg = erlang:binary_to_integer(?MODULE:id(UINT64MaxNegBin), 16),
    ok = ?MODULE:is_number_helper(?MODULE:id(UINT64MaxNeg)),
    true = is_number(?MODULE:id(UINT64MaxNeg)),

    INT63MaxP1Bin = <<"8000000000000000">>,
    INT63MaxP1 = erlang:binary_to_integer(?MODULE:id(INT63MaxP1Bin), 16),
    ok = ?MODULE:is_number_helper(?MODULE:id(INT63MaxP1)),
    true = is_number(?MODULE:id(INT63MaxP1)),

    INT63MinM1Bin = <<"-8000000000000001">>,
    INT63MinM1 = erlang:binary_to_integer(?MODULE:id(INT63MinM1Bin), 16),
    ok = ?MODULE:is_number_helper(?MODULE:id(INT63MinM1)),
    true = is_number(?MODULE:id(INT63MinM1)),

    MaxPatternBin = <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    MaxPattern = erlang:binary_to_integer(?MODULE:id(MaxPatternBin), 16),
    ok = ?MODULE:is_number_helper(?MODULE:id(MaxPattern)),
    true = is_number(?MODULE:id(MaxPattern)),

    RandomPatternBin = <<"4LWS1KF502AD5JUXQCS">>,
    RandomPattern = erlang:binary_to_integer(?MODULE:id(RandomPatternBin), 35),
    ok = ?MODULE:is_number_helper(?MODULE:id(RandomPattern)),
    true = is_number(?MODULE:id(RandomPattern)),

    0.

is_number_helper(N) when is_number(N) ->
    _ = ?MODULE:id(N),
    ?MODULE:id(ok);
is_number_helper(_N) ->
    ?MODULE:id(error).

test_gt_lt_guards() ->
    MaxPatternBin = <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    MaxPattern = erlang:binary_to_integer(?MODULE:id(MaxPatternBin), 16),
    non_negative = ?MODULE:classify1(?MODULE:id(MaxPattern)),
    positive = ?MODULE:classify2(?MODULE:id(MaxPattern)),

    MinPatternBin = <<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    MinPattern = erlang:binary_to_integer(?MODULE:id(MinPatternBin), 16),
    negative = ?MODULE:classify1(?MODULE:id(MinPattern)),
    negative = ?MODULE:classify2(?MODULE:id(MinPattern)),

    Pattern128Bin = <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    Pattern128 = erlang:binary_to_integer(?MODULE:id(Pattern128Bin), 16),
    non_negative = ?MODULE:classify1(?MODULE:id(Pattern128)),
    positive = ?MODULE:classify2(?MODULE:id(Pattern128)),

    Pattern128NegBin = <<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    Pattern128Neg = erlang:binary_to_integer(?MODULE:id(Pattern128NegBin), 16),
    negative = ?MODULE:classify1(?MODULE:id(Pattern128Neg)),
    negative = ?MODULE:classify2(?MODULE:id(Pattern128Neg)),

    UINT64MaxBin = <<"FFFFFFFFFFFFFFFF">>,
    UINT64Max = erlang:binary_to_integer(?MODULE:id(UINT64MaxBin), 16),
    non_negative = ?MODULE:classify1(?MODULE:id(UINT64Max)),
    positive = ?MODULE:classify2(?MODULE:id(UINT64Max)),

    UINT64MaxNegBin = <<"-FFFFFFFFFFFFFFFF">>,
    UINT64MaxNeg = erlang:binary_to_integer(?MODULE:id(UINT64MaxNegBin), 16),
    negative = ?MODULE:classify1(?MODULE:id(UINT64MaxNeg)),
    negative = ?MODULE:classify2(?MODULE:id(UINT64MaxNeg)),

    INT63MaxP1Bin = <<"8000000000000000">>,
    INT63MaxP1 = erlang:binary_to_integer(?MODULE:id(INT63MaxP1Bin), 16),
    non_negative = ?MODULE:classify1(?MODULE:id(INT63MaxP1)),
    positive = ?MODULE:classify2(?MODULE:id(INT63MaxP1)),

    INT63MinM1Bin = <<"-8000000000000001">>,
    INT63MinM1 = erlang:binary_to_integer(?MODULE:id(INT63MinM1Bin), 16),
    negative = ?MODULE:classify1(?MODULE:id(INT63MinM1)),
    negative = ?MODULE:classify2(?MODULE:id(INT63MinM1)),

    MaxPatternBin = <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    MaxPattern = erlang:binary_to_integer(?MODULE:id(MaxPatternBin), 16),
    non_negative = ?MODULE:classify1(?MODULE:id(MaxPattern)),
    positive = ?MODULE:classify2(?MODULE:id(MaxPattern)),

    RandomPatternBin = <<"4LWS1KF502AD5JUXQCS">>,
    RandomPattern = erlang:binary_to_integer(?MODULE:id(RandomPatternBin), 35),
    non_negative = ?MODULE:classify1(?MODULE:id(RandomPattern)),
    positive = ?MODULE:classify2(?MODULE:id(RandomPattern)),

    0.

classify1(X) when is_integer(X) andalso X >= 0 ->
    ?MODULE:fst(?MODULE:t2(non_negative, X));
classify1(X) when is_integer(X) ->
    ?MODULE:fst(?MODULE:t2(negative, ?MODULE:idB(X)));
classify1(X) ->
    _ = ?MODULE:id(X),
    error.

classify2(X) when is_integer(X) andalso X < 0 ->
    ?MODULE:fst(?MODULE:t2(negative, X));
classify2(X) when X =:= 0 ->
    ?MODULE:fst(?MODULE:t2(zero, ?MODULE:id(X)));
classify2(X) when is_integer(X) ->
    ?MODULE:fst(?MODULE:t2(positive, ?MODULE:idB(X)));
classify2(X) ->
    _ = ?MODULE:id(X),
    error.

to_external_term() ->
    % maximum
    <<131, 110, 32, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255>> = ?MODULE:id(
        erlang:term_to_binary(
            ?MODULE:id(
                erlang:binary_to_integer(
                    ?MODULE:id(
                        <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>
                    ),
                    16
                )
            )
        )
    ),

    % minimum
    <<131, 110, 32, 1, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255>> = ?MODULE:id(
        erlang:term_to_binary(
            ?MODULE:id(
                erlang:binary_to_integer(
                    ?MODULE:id(
                        <<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>
                    ),
                    16
                )
            )
        )
    ),

    % positive test pattern
    <<131, 110, 32, 0, 189, 121, 53, 209, 236, 251, 234, 208, 201, 184, 167, 86, 79, 62, 45, 28, 11,
        42, 49, 82, 116, 150, 248, 222, 188, 154, 120, 86, 52, 18, 254, 202>> = ?MODULE:id(
        erlang:term_to_binary(
            ?MODULE:id(
                erlang:binary_to_integer(
                    ?MODULE:id(
                        <<"CAFE123456789ABCDEF8967452312A0B1C2D3E4F56A7B8C9D0EAFBECD13579BD">>
                    ),
                    16
                )
            )
        )
    ),

    % negative test pattern
    <<131, 110, 32, 1, 189, 121, 53, 209, 236, 251, 234, 208, 201, 184, 167, 86, 79, 62, 45, 28, 11,
        42, 49, 82, 116, 150, 248, 222, 188, 154, 120, 86, 52, 18, 254, 202>> = ?MODULE:id(
        erlang:term_to_binary(
            ?MODULE:id(
                erlang:binary_to_integer(
                    ?MODULE:id(
                        <<"-CAFE123456789ABCDEF8967452312A0B1C2D3E4F56A7B8C9D0EAFBECD13579BD">>
                    ),
                    16
                )
            )
        )
    ),

    % test encoding multiple elements
    <<131, 108, 0, 0, 0, 2, 110, 32, 0, 189, 121, 53, 209, 236, 251, 234, 208, 201, 184, 167, 86,
        79, 62, 45, 28, 11, 42, 49, 82, 116, 150, 248, 222, 188, 154, 120, 86, 52, 18, 254, 202,
        109, 0, 0, 0, 3, 116, 115, 116, 106>> = ?MODULE:id(
        erlang:term_to_binary([
            ?MODULE:id(
                erlang:binary_to_integer(
                    ?MODULE:id(
                        <<"CAFE123456789ABCDEF8967452312A0B1C2D3E4F56A7B8C9D0EAFBECD13579BD">>
                    ),
                    16
                )
            ),
            ?MODULE:id(<<"tst">>)
        ])
    ),

    % length is 31 bytes long, not divisible by 4, this might cause buffer overflows
    % if not handled correctly
    <<131, 110, 31, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255>> = ?MODULE:id(
        erlang:term_to_binary(
            ?MODULE:id(
                erlang:binary_to_integer(
                    ?MODULE:id(
                        <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>
                    ),
                    16
                )
            )
        )
    ),

    % length is 27 bytes long, not disible by 4, also on 64 bits system there is a 0 digit once encoded as term
    % this might cause issues if not handled correctly
    <<131, 110, 27, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255>> = ?MODULE:id(
        erlang:term_to_binary(
            ?MODULE:id(
                erlang:binary_to_integer(
                    ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
                )
            )
        )
    ),

    % test if encoding multiple elements works
    <<131, 108, 0, 0, 0, 3, 97, 1, 110, 27, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 109, 0,
        0, 0, 6, 116, 115, 116, 98, 105, 110, 106>> = ?MODULE:id(
        erlang:term_to_binary(
            ?MODULE:id([
                1,
                ?MODULE:id(
                    erlang:binary_to_integer(
                        ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
                    )
                ),
                <<"tstbin">>
            ])
        )
    ),

    0.

test_band() ->
    MaxPatternBin = <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    MaxPattern = erlang:binary_to_integer(?MODULE:id(MaxPatternBin), 16),

    % Following are converted using base 10
    <<"0">> = erlang:integer_to_binary(?MODULE:id(?MODULE:id(0) band ?MODULE:id(MaxPattern))),
    <<"1">> = erlang:integer_to_binary(?MODULE:id(?MODULE:id(1) band ?MODULE:id(MaxPattern))),
    <<"42">> = erlang:integer_to_binary(?MODULE:id(?MODULE:id(42) band ?MODULE:id(MaxPattern))),

    % base 16 again
    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(-1) band ?MODULE:id(MaxPattern)), 16
    ),
    MaxPatternBin = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(-1) band ?MODULE:id(MaxPattern)), 16
    ),
    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD6">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(-42) band ?MODULE:id(MaxPattern)), 16
    ),

    Pattern1Bin = <<"ABCDEF01234567891A2B3C4D5E6F7A8B9C0987654321FEDCBA1133557799AABB">>,
    Pattern1 = erlang:binary_to_integer(?MODULE:id(Pattern1Bin), 16),
    Pattern2Bin = <<"-ABCDEF01234567891A2B3C4D5E6F7A8B9C0987654321FEDCBA1133557799AABB">>,
    Pattern2 = erlang:binary_to_integer(?MODULE:id(Pattern2Bin), 16),
    Pattern3Bin = <<"429F7B79E176813134266B08934B692D150E2256A5622164F5E71321FC02A7B6">>,
    Pattern3 = erlang:binary_to_integer(?MODULE:id(Pattern3Bin), 16),
    Pattern4Bin = <<"7D4BEFE3454125529A5C377D7D02D005B4ABA5C133FEB2768E0A04A610735D88">>,
    Pattern4 = erlang:binary_to_integer(?MODULE:id(Pattern4Bin), 16),
    Pattern5Bin = <<"C617C2D4AD3FA4331BAD932538A828460E5D55FCAC2444154AA37E60EFEB7351">>,
    Pattern5 = erlang:binary_to_integer(?MODULE:id(Pattern5Bin), 16),
    Pattern6Bin = <<"-DBD6308C83498D7C8B5327507D10C30974CB034EEB514EFE4D85E044B5BF25DC">>,
    Pattern6 = erlang:binary_to_integer(?MODULE:id(Pattern6Bin), 16),

    <<"ABCDEF01234567891A2B3C4D5E6F7A8B9C0987654321FEDCBA1133557799AABB">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern1) band ?MODULE:id(Pattern1)), 16
    ),
    <<"1">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern1) band ?MODULE:id(Pattern2)), 16
    ),
    <<"-ABCDEF01234567891A2B3C4D5E6F7A8B9C0987654321FEDCBA1133557799AABB">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern2) band ?MODULE:id(Pattern2)), 16
    ),
    <<"40121078C0328030240443008100012401062012A442012045E6002088020504">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern2) band ?MODULE:id(Pattern3)), 16
    ),
    <<"400B6B61414001101004230811024005140A2040216220648402002010020580">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern3) band ?MODULE:id(Pattern4)), 16
    ),
    <<"4403C2C0050124121A0C132538000004040905C0202400140A02042000635100">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern4) band ?MODULE:id(Pattern5)), 16
    ),
    <<"401C2502C36200310AC902500A828460A1454B00424000102221E204A405200">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern5) band ?MODULE:id(Pattern6)), 16
    ),
    <<"401C2502C36200310AC902500A828460A1454B00424000102221E204A405200">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern6) band ?MODULE:id(Pattern5)), 16
    ),
    <<"401C2502C36200310AC902500A828460A1454B00424000102221E204A405200">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern5) band ?MODULE:id(Pattern6)), 16
    ),
    <<"-DBD6308C83498D7C8B5327507D10C30974CB034EEB514EFE4D85E044B5BF25DC">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern6) band ?MODULE:id(Pattern6)), 16
    ),
    <<"-FBDFFF8DA34DEFFD9B7B3F5D7F7FFB8BFCCB876FEB71FEFEFF95F355F7BFAFFC">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern2) band ?MODULE:id(Pattern6)), 16
    ),
    <<"-FBDFFF8DA34DEFFD9B7B3F5D7F7FFB8BFCCB876FEB71FEFEFF95F355F7BFAFFC">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern6) band ?MODULE:id(Pattern2)), 16
    ),

    Pattern7Bin = <<"-8000000000000000000000000000000000000000000000000000000000000000">>,
    Pattern7 = erlang:binary_to_integer(?MODULE:id(Pattern7Bin), 16),
    Pattern8Bin = <<"-4000000000000000000000000000000000000000000000000000000000000000">>,
    Pattern8 = erlang:binary_to_integer(?MODULE:id(Pattern8Bin), 16),

    <<"-8000000000000000000000000000000000000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern7) band ?MODULE:id(Pattern8)), 16
    ),

    <<"-8000000000000000000000000000000000000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern8) band ?MODULE:id(Pattern7)), 16
    ),

    Pattern9Bin = <<"-8000000000000000000000000000000000000000000000000000000000000000">>,
    Pattern9 = erlang:binary_to_integer(?MODULE:id(Pattern9Bin), 16),
    Pattern10Bin = <<"4000000000000000000000000000000000000000000000000000000000000000">>,
    Pattern10 = erlang:binary_to_integer(?MODULE:id(Pattern10Bin), 16),

    <<"0">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern9) band ?MODULE:id(Pattern10)), 16
    ),

    <<"0">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern10) band ?MODULE:id(Pattern9)), 16
    ),

    Pattern11Bin = <<"FFFFFFFFFFFFFFFFF">>,
    Pattern11 = erlang:binary_to_integer(?MODULE:id(Pattern11Bin), 16),
    Pattern12Bin = <<"F00FFFFFFFFFFFFFFFF">>,
    Pattern12 = erlang:binary_to_integer(?MODULE:id(Pattern12Bin), 16),

    <<"FFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern11) band ?MODULE:id(Pattern12)), 16
    ),

    Pattern13Bin = <<"FFF1FFFFFFFFFFFF">>,
    Pattern13 = erlang:binary_to_integer(?MODULE:id(Pattern13Bin), 16),
    Pattern14Bin = <<"FFFFFFFFFFF5FFFF">>,
    Pattern14 = erlang:binary_to_integer(?MODULE:id(Pattern14Bin), 16),

    <<"FFF1FFFFFFF5FFFF">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern13) band ?MODULE:id(Pattern14)), 16
    ),

    Pattern15 = erlang:binary_to_integer(?MODULE:id(<<"80008000">>), 16),
    Pattern16 = erlang:binary_to_integer(
        ?MODULE:id(<<"7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    <<"80008000">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern15) band ?MODULE:id(Pattern16)), 16
    ),

    0.

test_bxor() ->
    MaxPatternBin = <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>,
    MaxPattern = erlang:binary_to_integer(?MODULE:id(MaxPatternBin), 16),

    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(0) bxor ?MODULE:id(MaxPattern)), 16
    ),
    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(1) bxor ?MODULE:id(MaxPattern)), 16
    ),
    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(5) bxor ?MODULE:id(MaxPattern)), 16
    ),

    % Here the behaviour differs from the BEAM
    % The BEAM has an "unlimited" big integers, so it is always possible to build a bigger one
    % without any loss of information.
    % AtomVM is limited to 256 bit + sign, so the sign bit might be discarded in some specific
    % situations, since it is not possible to build a "more negative" 257 bit integer.
    Res1 = choose_result(
        <<"0">>, <<"-10000000000000000000000000000000000000000000000000000000000000000">>
    ),
    Res1 = erlang:integer_to_binary(?MODULE:id(?MODULE:id(-1) bxor ?MODULE:id(MaxPattern)), 16),
    Res1 = erlang:integer_to_binary(?MODULE:id(?MODULE:id(MaxPattern) bxor ?MODULE:id(-1)), 16),

    Pattern1Bin = <<"ABCDEF01234567891A2B3C4D5E6F7A8B9C0987654321FEDCBA1133557799AABB">>,
    Pattern1 = erlang:binary_to_integer(?MODULE:id(Pattern1Bin), 16),
    Pattern2Bin = <<"-ABCDEF01234567891A2B3C4D5E6F7A8B9C0987654321FEDCBA1133557799AABB">>,
    Pattern2 = erlang:binary_to_integer(?MODULE:id(Pattern2Bin), 16),
    Pattern3Bin = <<"429F7B79E176813134266B08934B692D150E2256A5622164F5E71321FC02A7B6">>,
    Pattern3 = erlang:binary_to_integer(?MODULE:id(Pattern3Bin), 16),
    Pattern4Bin = <<"7D4BEFE3454125529A5C377D7D02D005B4ABA5C133FEB2768E0A04A610735D88">>,
    Pattern4 = erlang:binary_to_integer(?MODULE:id(Pattern4Bin), 16),
    Pattern5Bin = <<"C617C2D4AD3FA4331BAD932538A828460E5D55FCAC2444154AA37E60EFEB7351">>,
    Pattern5 = erlang:binary_to_integer(?MODULE:id(Pattern5Bin), 16),
    Pattern6Bin = <<"-DBD6308C83498D7C8B5327507D10C30974CB034EEB514EFE4D85E044B5BF25DC">>,
    Pattern6 = erlang:binary_to_integer(?MODULE:id(Pattern6Bin), 16),
    Pattern7Bin = <<"1FE2315B2ED07E444FD674612917C4EA">>,
    Pattern7 = erlang:binary_to_integer(?MODULE:id(Pattern7Bin), 16),
    Pattern8Bin = <<"66291789880994637C2DDCE876C62C32">>,
    Pattern8 = erlang:binary_to_integer(?MODULE:id(Pattern8Bin), 16),
    Pattern9Bin = <<"-51E890688B984C76550C33A169F41C1E">>,
    Pattern9 = erlang:binary_to_integer(?MODULE:id(Pattern9Bin), 16),

    <<"0">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern1) bxor ?MODULE:id(Pattern1)), 16
    ),
    <<"-2">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern1) bxor ?MODULE:id(Pattern2)), 16
    ),
    <<"0">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern2) bxor ?MODULE:id(Pattern2)), 16
    ),
    <<"-E9529478C233E6B82E0D5745CD2413A68907A533E643DFB84FF620748B9B0D0D">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern2) bxor ?MODULE:id(Pattern3)), 16
    ),
    <<"3FD4949AA437A463AE7A5C75EE49B928A1A58797969C93127BED1787EC71FA3E">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern3) bxor ?MODULE:id(Pattern4)), 16
    ),
    <<"BB5C2D37E87E816181F1A45845AAF843BAF6F03D9FDAF663C4A97AC6FF982ED9">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern4) bxor ?MODULE:id(Pattern5)), 16
    ),
    <<"-1DC1F2582E76294F90FEB47545B8EB4F7A9656B247750AEB07269E245A54568B">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern5) bxor ?MODULE:id(Pattern6)), 16
    ),
    <<"0">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern6) bxor ?MODULE:id(Pattern6)), 16
    ),
    <<"79CB26D2A6D9EA2733FBA8895FD1E8D8">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern7) bxor ?MODULE:id(Pattern8)), 16
    ),
    <<"-37C187E10391D8152921EF491F323030">> = erlang:integer_to_binary(
        ?MODULE:id(?MODULE:id(Pattern8) bxor ?MODULE:id(Pattern9)), 16
    ),

    0.

test_bor() ->
    Pattern1 = erlang:binary_to_integer(
        ?MODULE:id(
            <<"10101010101010101010101010101010101010101010101010101010101010100000000000000000">>
        ),
        2
    ),
    Pattern2 = erlang:binary_to_integer(
        ?MODULE:id(
            <<"1010101010101010101010101010101010101010101010101010101010101010000000000000000">>
        ),
        2
    ),
    Res1 = erlang:binary_to_integer(
        ?MODULE:id(
            <<"11111111111111111111111111111111111111111111111111111111111111110000000000000000">>
        ),
        2
    ),
    Res1 = Pattern1 bor Pattern2,

    Pattern3 = erlang:binary_to_integer(?MODULE:id(<<"-1">>), 2),
    Res2 = ?MODULE:id(-1),
    Res2 = Pattern1 bor Pattern3,

    Pattern4 = erlang:binary_to_integer(?MODULE:id(<<"-5555555511111111123456789ABCDEF0">>), 16),
    Pattern5 = erlang:binary_to_integer(?MODULE:id(<<"+30303030333333333111111111111111">>), 16),
    Res3 = erlang:binary_to_integer(?MODULE:id(<<"-4545454500000000022446688AACCEEF">>), 16),
    Res3 = Pattern4 bor Pattern5,

    Pattern6 = erlang:binary_to_integer(?MODULE:id(<<"-30303030333333333111111111111111">>), 16),
    Res4 = erlang:binary_to_integer(?MODULE:id(<<"-10101010111111111010101010101001">>), 16),
    Res4 = Pattern4 bor Pattern6,

    Pattern7 = erlang:binary_to_integer(
        ?MODULE:id(<<"-8000000000000000000000000000000000000000000000000000000000000000">>), 16
    ),
    Res5 = ?MODULE:id(-1),
    Res5 = ?MODULE:id(Pattern7) bor ?MODULE:id(-1),

    Res6 = Pattern4,
    Res6 = ?MODULE:id(Pattern4) bor ?MODULE:id(Pattern7),

    Res7 = erlang:binary_to_integer(
        ?MODULE:id(<<"-7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    Res7 = ?MODULE:id(Pattern7) bor ?MODULE:id(1),

    Pattern8 = erlang:binary_to_integer(
        ?MODULE:id(<<"5555555555555555555555555555555555555555555555555555555555555555">>), 16
    ),
    Pattern9 = erlang:binary_to_integer(
        ?MODULE:id(<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>), 16
    ),
    Res8 = erlang:binary_to_integer(
        ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    Res8 = Pattern8 bor Pattern9,

    Res9 = ?MODULE:id(-1),
    Res9 = ?MODULE:id(-1) bor Res8,

    0.

test_bsl() ->
    Pattern1 = erlang:binary_to_integer(?MODULE:id(<<"CAFE1234AABBCCDD98765432">>), 16),
    <<"CAFE1234AABBCCDD98765432000000">> = erlang:integer_to_binary(
        Pattern1 bsl ?MODULE:id(24), 16
    ),
    <<"195FC2469557799BB30ECA8640000000">> = erlang:integer_to_binary(
        Pattern1 bsl ?MODULE:id(29), 16
    ),
    <<"CAFE1234AABBCCDD9876543200000000">> = erlang:integer_to_binary(
        Pattern1 bsl ?MODULE:id(32), 16
    ),
    <<"657F091A555DE66ECC3B2A19000000000">> = erlang:integer_to_binary(
        Pattern1 bsl ?MODULE:id(35), 16
    ),
    <<"CAFE1234AABBCCDD98765432000000000000">> = erlang:integer_to_binary(
        Pattern1 bsl ?MODULE:id(48), 16
    ),
    <<"657F091A555DE66ECC3B2A190000000000000">> = erlang:integer_to_binary(
        Pattern1 bsl ?MODULE:id(51), 16
    ),
    <<"CAFE1234AABBCCDD987654320000000000000000">> = erlang:integer_to_binary(
        Pattern1 bsl ?MODULE:id(64), 16
    ),
    <<"CAFE1234AABBCCDD987654320000000000000000000000000000000000000000">> = erlang:integer_to_binary(
        Pattern1 bsl ?MODULE:id(160), 16
    ),
    <<"657F00000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(16#CAFE) bsl ?MODULE:id(127), 16
    ),
    <<"CAFE00000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(16#CAFE) bsl ?MODULE:id(128), 16
    ),
    <<"195FC00000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(16#CAFE) bsl ?MODULE:id(129), 16
    ),
    <<"CAFE000000000000000000000000000000000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(16#CAFE) bsl ?MODULE:id(240), 16
    ),

    Pattern2 = erlang:binary_to_integer(?MODULE:id(<<"-CAFE1234AABBCCDD98765432">>), 16),
    <<"-CAFE1234AABBCCDD98765432000000">> = erlang:integer_to_binary(
        Pattern2 bsl ?MODULE:id(24), 16
    ),
    <<"-195FC2469557799BB30ECA8640000000">> = erlang:integer_to_binary(
        Pattern2 bsl ?MODULE:id(29), 16
    ),
    <<"-CAFE1234AABBCCDD9876543200000000">> = erlang:integer_to_binary(
        Pattern2 bsl ?MODULE:id(32), 16
    ),
    <<"-657F091A555DE66ECC3B2A19000000000">> = erlang:integer_to_binary(
        Pattern2 bsl ?MODULE:id(35), 16
    ),
    <<"-CAFE1234AABBCCDD98765432000000000000">> = erlang:integer_to_binary(
        Pattern2 bsl ?MODULE:id(48), 16
    ),
    <<"-657F091A555DE66ECC3B2A190000000000000">> = erlang:integer_to_binary(
        Pattern2 bsl ?MODULE:id(51), 16
    ),
    <<"-CAFE1234AABBCCDD987654320000000000000000">> = erlang:integer_to_binary(
        Pattern2 bsl ?MODULE:id(64), 16
    ),
    <<"-CAFE1234AABBCCDD987654320000000000000000000000000000000000000000">> = erlang:integer_to_binary(
        Pattern2 bsl ?MODULE:id(160), 16
    ),
    <<"-657F00000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(-16#CAFE) bsl ?MODULE:id(127), 16
    ),
    <<"-CAFE00000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(-16#CAFE) bsl ?MODULE:id(128), 16
    ),
    <<"-195FC00000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(-16#CAFE) bsl ?MODULE:id(129), 16
    ),
    <<"-CAFE000000000000000000000000000000000000000000000000000000000000">> = erlang:integer_to_binary(
        ?MODULE:id(-16#CAFE) bsl ?MODULE:id(240), 16
    ),

    LS1 = erlang:binary_to_integer(
        ?MODULE:id(<<"CAFE000000000000000000000000000000000000000000000000000000000001">>), 16
    ),
    ok = expect_overflow_or_limit(fun() -> ?MODULE:id(?MODULE:id(5) bsl ?MODULE:id(LS1)) end),
    ok = expect_overflow_or_limit(fun() -> ?MODULE:id(?MODULE:id(-1) bsl ?MODULE:id(LS1)) end),
    0 = ?MODULE:id(?MODULE:id(0) bsl ?MODULE:id(LS1)),

    LS2 = erlang:binary_to_integer(?MODULE:id(<<"4000000000000000">>), 16),
    ok = expect_overflow_or_limit(fun() -> ?MODULE:id(?MODULE:id(5) bsl ?MODULE:id(LS2)) end),
    ok = expect_overflow_or_limit(fun() -> ?MODULE:id(?MODULE:id(-1) bsl ?MODULE:id(LS2)) end),
    ok = expect_overflow_or_limit(fun() ->
        ?MODULE:id(?MODULE:id(5) bsl ?MODULE:id(16#0000FFFF00000002))
    end),
    0 = ?MODULE:id(?MODULE:id(0) bsl ?MODULE:id(LS2)),

    % Negative bsl is bsr
    Pattern3 = erlang:binary_to_integer(?MODULE:id(<<"CAFE1234AABBCCDD98765432987654321">>), 16),
    <<"CAFE1234AABBCCDD98765432987">> = erlang:integer_to_binary(Pattern3 bsl ?MODULE:id(-24), 16),

    NLS1 = erlang:binary_to_integer(
        ?MODULE:id(<<"-CAFE000000000000000000000000000000000000000000000000000000000001">>), 16
    ),
    0 = ?MODULE:id(?MODULE:id(5) bsl ?MODULE:id(NLS1)),
    -1 = ?MODULE:id(?MODULE:id(-1) bsl ?MODULE:id(NLS1)),
    0 = ?MODULE:id(?MODULE:id(0) bsl ?MODULE:id(NLS1)),

    NLS2 = erlang:binary_to_integer(?MODULE:id(<<"-4000000000000000">>), 16),
    0 = ?MODULE:id(?MODULE:id(5) bsl ?MODULE:id(NLS2)),
    -1 = ?MODULE:id(?MODULE:id(-1) bsl ?MODULE:id(NLS2)),
    0 = ?MODULE:id(?MODULE:id(0) bsl ?MODULE:id(NLS2)),

    0.

test_bsr() ->
    Pattern1 = erlang:binary_to_integer(?MODULE:id(<<"CAFE1234AABBCCDD98765432987654321">>), 16),
    <<"CAFE1234AABBCCDD98765432987">> = erlang:integer_to_binary(Pattern1 bsr ?MODULE:id(24), 16),
    <<"657F091A555DE66ECC3B2A194C">> = erlang:integer_to_binary(Pattern1 bsr ?MODULE:id(29), 16),
    <<"CAFE1234AABBCCDD987654329">> = erlang:integer_to_binary(Pattern1 bsr ?MODULE:id(32), 16),
    <<"195FC2469557799BB30ECA865">> = erlang:integer_to_binary(Pattern1 bsr ?MODULE:id(35), 16),
    <<"CAFE1234AABBCCDD98765">> = erlang:integer_to_binary(Pattern1 bsr ?MODULE:id(48), 16),
    <<"195FC2469557799BB30EC">> = erlang:integer_to_binary(Pattern1 bsr ?MODULE:id(51), 16),
    <<"CAFE1234AABBCCDD9">> = erlang:integer_to_binary(Pattern1 bsr ?MODULE:id(64), 16),
    <<"C">> = erlang:integer_to_binary(Pattern1 bsr ?MODULE:id(128), 16),
    <<"0">> = erlang:integer_to_binary(Pattern1 bsr ?MODULE:id(250), 16),
    <<"0">> = erlang:integer_to_binary(Pattern1 bsr ?MODULE:id(256), 16),
    <<"0">> = erlang:integer_to_binary(Pattern1 bsr ?MODULE:id(257), 16),
    <<"0">> = erlang:integer_to_binary(Pattern1 bsr ?MODULE:id(600), 16),

    Pattern2 = erlang:binary_to_integer(?MODULE:id(<<"-CAFE1234AABBCCDD98765432987654321">>), 16),
    <<"-CAFE1234AABBCCDD98765432988">> = erlang:integer_to_binary(Pattern2 bsr ?MODULE:id(24), 16),
    <<"-657F091A555DE66ECC3B2A194D">> = erlang:integer_to_binary(Pattern2 bsr ?MODULE:id(29), 16),
    <<"-CAFE1234AABBCCDD98765432A">> = erlang:integer_to_binary(Pattern2 bsr ?MODULE:id(32), 16),
    <<"-195FC2469557799BB30ECA866">> = erlang:integer_to_binary(Pattern2 bsr ?MODULE:id(35), 16),
    <<"-CAFE1234AABBCCDD98766">> = erlang:integer_to_binary(Pattern2 bsr ?MODULE:id(48), 16),
    <<"-195FC2469557799BB30ED">> = erlang:integer_to_binary(Pattern2 bsr ?MODULE:id(51), 16),
    <<"-CAFE1234AABBCCDDA">> = erlang:integer_to_binary(Pattern2 bsr ?MODULE:id(64), 16),
    <<"-D">> = erlang:integer_to_binary(Pattern2 bsr ?MODULE:id(128), 16),
    <<"-1">> = erlang:integer_to_binary(Pattern2 bsr ?MODULE:id(250), 16),
    <<"-1">> = erlang:integer_to_binary(Pattern2 bsr ?MODULE:id(256), 16),
    <<"-1">> = erlang:integer_to_binary(Pattern2 bsr ?MODULE:id(257), 16),
    <<"-1">> = erlang:integer_to_binary(Pattern2 bsr ?MODULE:id(600), 16),

    LS1 = erlang:binary_to_integer(
        ?MODULE:id(<<"CAFE000000000000000000000000000000000000000000000000000000000001">>), 16
    ),
    0 = ?MODULE:id(?MODULE:id(5) bsr ?MODULE:id(LS1)),
    -1 = ?MODULE:id(?MODULE:id(-1) bsr ?MODULE:id(LS1)),
    0 = ?MODULE:id(?MODULE:id(0) bsr ?MODULE:id(LS1)),

    LS2 = erlang:binary_to_integer(?MODULE:id(<<"4000000000000000">>), 16),
    0 = ?MODULE:id(?MODULE:id(5) bsr ?MODULE:id(LS2)),
    -1 = ?MODULE:id(?MODULE:id(-1) bsr ?MODULE:id(LS2)),
    0 = ?MODULE:id(?MODULE:id(0) bsr ?MODULE:id(LS2)),

    0 = ?MODULE:id(?MODULE:id(5) bsr ?MODULE:id(16#0000FFFF00000002)),
    -1 = ?MODULE:id(?MODULE:id(-5) bsr ?MODULE:id(16#0000FFFF00000002)),

    % Negative bsr is bsl
    Pattern3 = erlang:binary_to_integer(?MODULE:id(<<"CAFE1234AABBCCDD98765432">>), 16),
    <<"CAFE1234AABBCCDD98765432000000">> = erlang:integer_to_binary(
        Pattern3 bsr ?MODULE:id(-24), 16
    ),

    NLS1 = erlang:binary_to_integer(
        ?MODULE:id(<<"-CAFE000000000000000000000000000000000000000000000000000000000001">>), 16
    ),
    ok = expect_overflow_or_limit(fun() -> ?MODULE:id(?MODULE:id(5) bsr ?MODULE:id(NLS1)) end),
    ok = expect_overflow_or_limit(fun() -> ?MODULE:id(?MODULE:id(-1) bsr ?MODULE:id(NLS1)) end),
    0 = ?MODULE:id(?MODULE:id(0) bsr ?MODULE:id(LS1)),

    NLS2 = erlang:binary_to_integer(?MODULE:id(<<"-4000000000000000">>), 16),
    ok = expect_overflow_or_limit(fun() -> ?MODULE:id(?MODULE:id(5) bsr ?MODULE:id(NLS2)) end),
    ok = expect_overflow_or_limit(fun() -> ?MODULE:id(?MODULE:id(-1) bsr ?MODULE:id(NLS2)) end),
    0 = ?MODULE:id(?MODULE:id(0) bsr ?MODULE:id(NLS2)),

    0.

test_bnot() ->
    Pattern1 = erlang:binary_to_integer(?MODULE:id(<<"CAFE1234AABBCCDD98765432987654321">>), 16),
    Pattern2 = erlang:binary_to_integer(?MODULE:id(<<"-CAFE1234AABBCCDD98765432987654321">>), 16),
    <<"-CAFE1234AABBCCDD98765432987654322">> = integer_to_binary(
        ?MODULE:id(bnot (?MODULE:id(Pattern1))), 16
    ),
    <<"CAFE1234AABBCCDD98765432987654320">> = integer_to_binary(
        ?MODULE:id(bnot (?MODULE:id(Pattern2))), 16
    ),

    Pattern3 = erlang:binary_to_integer(
        ?MODULE:id(<<"7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    Pattern4 = erlang:binary_to_integer(
        ?MODULE:id(<<"-7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),

    <<"-8000000000000000000000000000000000000000000000000000000000000000">> = integer_to_binary(
        ?MODULE:id(bnot (?MODULE:id(Pattern3))), 16
    ),
    <<"7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE">> = integer_to_binary(
        ?MODULE:id(bnot (?MODULE:id(Pattern4))), 16
    ),

    PatternMax = erlang:binary_to_integer(
        ?MODULE:id(<<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),
    PatternMin = erlang:binary_to_integer(
        ?MODULE:id(<<"-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF">>), 16
    ),

    % Here the behaviour differs from the BEAM
    % See previous comment on this topic
    NotPatternMax = choose_result(
        <<"0">>, <<"-10000000000000000000000000000000000000000000000000000000000000000">>
    ),
    NotPatternMax = integer_to_binary(?MODULE:id(bnot (?MODULE:id(PatternMax))), 16),
    <<"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE">> = integer_to_binary(
        ?MODULE:id(bnot (?MODULE:id(PatternMin))), 16
    ),

    0.

id(X) ->
    X.

idB(X) ->
    X.

t2(A, B) ->
    {A, B}.

fst({A, _B}) ->
    A.

choose_result(AResult, BResult) ->
    case get_machine_atom() of
        atomvm -> AResult;
        beam -> BResult
    end.

expect_atomvm_error(Error, ErrFun) ->
    Machine = ?MODULE:get_machine_atom(),
    try {Machine, ErrFun()} of
        {beam, _I} -> ok;
        {atomvm, Result} -> {unexpected_result, Result}
    catch
        error:Error -> ok;
        _:E -> {unexpected_error, E}
    end.

expect_overflow(OvfFun) ->
    expect_atomvm_error(overflow, OvfFun).

expect_overflow_or_limit(OvfFun) ->
    try OvfFun() of
        {atomvm, Result} -> {unexpected_result, Result}
    catch
        error:overflow -> ok;
        error:system_limit -> ok;
        _:E -> {unexpected_error, E}
    end.

expect_error(Error, BadFun) ->
    try BadFun() of
        Result -> {unexpected_result, Result}
    catch
        error:Error -> ok;
        _:E -> {unexpected_error, E}
    end.

get_machine_atom() ->
    case erlang:system_info(machine) of
        "BEAM" -> beam;
        _ -> atomvm
    end.
