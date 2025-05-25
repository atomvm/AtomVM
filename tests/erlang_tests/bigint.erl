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
    expect_badarg/1,
    expect_overflow/1,
    id/1
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
        parse_bigint() +
        test_cmp() +
        conv_to_from_float() +
        external_term_decode() +
        big_literals() +
        to_external_term() +
        test_band() +
        test_bxor() +
        test_bor().

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

    expect_badarg(fun() ->
        binary_to_integer(
            ?MODULE:id(
                <<"-45342150622142553455515645002565446330401366441046314643126036505535454140120366515240023z6">>
            ),
            7
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
    expect_overflow(fun() -> trunc(?MODULE:id(1.157920892373163e77)) end),
    expect_overflow(fun() -> trunc(?MODULE:id(-1.157920892373163e77)) end),

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
    0.

big_literals() ->
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

    % this cannot be tested
    % bigger literals, such as the one here, are encoded using an external term
    % (having SMALL_BIG_EXT type).
    % The reader function is not able to distinguish between different kind of invalid
    % errors, such as overflow, so this cannot be tested.
    % ok = expect_overflow(fun ?MODULE:lit_ovf1/0),
    % ok = expect_overflow(fun ?MODULE:lit_ovf2/0),

    0.

lit_ovf1() ->
    ?MODULE:id(16#10000000000000000000000000000000000000000000000000000000000000000).

lit_ovf2() ->
    ?MODULE:id(-16#10000000000000000000000000000000000000000000000000000000000000000).

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

id(X) ->
    X.

choose_result(AResult, BResult) ->
    case get_machine_atom() of
        atomvm -> AResult;
        beam -> BResult
    end.

expect_overflow(OvfFun) ->
    Machine = ?MODULE:get_machine_atom(),
    try {Machine, OvfFun()} of
        {beam, I} when is_integer(I) -> ok;
        {atomvm, Result} -> {unexpected_result, Result}
    catch
        error:overflow -> ok;
        _:E -> {unexpected_error, E}
    end.

expect_badarg(BadFun) ->
    try BadFun() of
        Result -> {unexpected_result, Result}
    catch
        error:badgarg -> ok;
        _:E -> {unexpected_error, E}
    end.

get_machine_atom() ->
    case erlang:system_info(machine) of
        "BEAM" -> beam;
        _ -> atomvm
    end.
