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
        test_cmp().

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

id(X) ->
    X.

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
