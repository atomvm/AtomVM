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

-module(bigint_stress).
-export([
    start/0,
    id/1
]).

%
% IMPORTANT NOTE
% AtomVM supports up to 256-bit integers with an additional sign bit stored outside the numeric
% payload, allowing for efficient representation of both signed and unsigned values without using
% two's complement encoding. So INT_MAX = -INT_MIN, that is:
% 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
%

-define(MAX_256, 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).

start() ->
    test_arith() +
        test_pow_mod() +
        test_lcg() +
        test_count_pop() +
        test_bloom() +
        test_xorshift256() +
        test_gf256() +
        test_bit_rotation() +
        test_uuid() +
        test_gray_code_with_not().

% arithmetic operations: tests addition, subtraction and multiplication
test_arith() ->
    A = weighted_diff(
        weighted_sum(
            ?MODULE:id(36#NNK6QZKA5WHTYVA1K48),
            ?MODULE:id(36#2AFHJ61MU4D94),
            ?MODULE:id(-36#2AFHJ61MU4D94),
            ?MODULE:id(36#3AFHJ61MU4D94)
        ),
        weighted_sum(
            ?MODULE:id(-36#GBNL1ZJ2WFGENENP27K65MTQ),
            ?MODULE:id(36#DSIMJUEQ7UIW3ZC8H5JS55IO),
            ?MODULE:id(-36#14N2FCHA3HRMKKVX4WB9GK2MNK),
            ?MODULE:id(36#2D2YHUCBN7DIO)
        )
    ),
    B = weighted_diff(
        weighted_sum(
            ?MODULE:id(36#IQT1DQ),
            ?MODULE:id(36#9KAOZN1V5HOJLQ),
            ?MODULE:id(-36#3V44Y91GFAMMV),
            ?MODULE:id(36#1VIAWBVDQZIYA)
        ),
        weighted_sum(
            ?MODULE:id(36#9EL4RX3BJNBM6WSY9DQXW8HIXQU2ZH0EAOXUO5B),
            ?MODULE:id(36#16T1N8OQZ35DQ6NUFVFMSOMGI8AQ),
            ?MODULE:id(-36#3WJYSSQL97IDRPR7N3),
            ?MODULE:id(36#395QFDOB79GK7TXFINGZF4ELIN)
        )
    ),
    C = weighted_diff(
        weighted_sum(
            ?MODULE:id(36#HT4QUI5ZQ4PV5TVFRXTUU660FOHTLI5T8),
            7,
            ?MODULE:id(-36#3V44Y91GFAMMV),
            ?MODULE:id(-36#2EVHLJPC6JBWCDZQ573)
        ),
        weighted_sum(
            -1,
            ?MODULE:id(36#2EVHLJPC6JBWCDZQ573),
            0,
            ?MODULE:id(36#395QFDOB79GK7TXFINGZF4ELIN)
        )
    ),
    D = weighted_diff(
        weighted_sum(
            ?MODULE:id(36#X8G0ZUYUZUV9),
            200,
            ?MODULE:id(36#4TDU20X4ZSBNP),
            ?MODULE:id(-36#2EVHLJPC6JBWCDZQ573)
        ),
        weighted_sum(
            -17,
            ?MODULE:id(36#BKF840SON41OONCK86T1JG5MVDU6ASSX5H2SH17),
            16#CAFECAFECAFECAFE,
            ?MODULE:id(36#MWJV8HK85PFBSMJ4JDPPAMTBLSS2IH516XO5892JPQUR6P1U)
        )
    ),

    sum_all(A, 1, B, 2, C, 3, D, 4, 0) +
        17639883425097572029428759870344464322120212138354185273272067489917369702365.

sum_all(A, B, C, D, E, F, G, H, I) ->
    A + B + C + D + E + F + G + H + I.

weighted_sum(A, B, C, D) when
    is_integer(A) andalso is_integer(B) andalso is_integer(C) andalso is_integer(D)
->
    A * 2 + B * 5 + C * 7 + D * 11;
weighted_sum(_A, _B, _C, _D) ->
    0.

weighted_diff(A, B) when is_integer(A) andalso is_integer(B) ->
    A * 2 - B * 5;
weighted_diff(_A, _B) ->
    0.

% modular exponentiation: tests arithmetic operations (including rem, div) and bitwise operations
test_pow_mod() ->
    24 = pow_mod(2, 10, 1000),
    81 = pow_mod(?MODULE:id(51), ?MODULE:id(27), ?MODULE:id(270)),
    2281 = pow_mod(?MODULE:id(11), ?MODULE:id(89), ?MODULE:id(4007)),

    P = ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F),
    1 = pow_mod(7, P - 1, P),
    4294968273 = pow_mod(2, 256, P),

    P192 = ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF),
    1 = pow_mod(11, P192 - 1, P192),
    1267650600228229401496703205376 = pow_mod(2, 100, P192),

    P224 = ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000001),
    1 = pow_mod(89, P224 - 1, P224),

    0.

pow_mod(_Base, 0, _Mod) ->
    1;
pow_mod(_Base, _Exp, Mod) when Mod =< 0 ->
    error(invalid_modulus);
pow_mod(Base, Exp, Mod) when
    is_integer(Base) andalso is_integer(Exp) andalso is_integer(Mod) andalso Exp < 0
->
    BaseInv = mod_inverse(Base, Mod),
    pow_mod(BaseInv, -Exp, Mod);
pow_mod(Base, Exp, Mod) when is_integer(Base) andalso is_integer(Exp) andalso is_integer(Mod) ->
    B = Base rem Mod,
    pow_mod_iter(B, Exp, Mod, 1).

pow_mod_iter(_Base, 0, _Mod, Acc) ->
    Acc;
pow_mod_iter(Base, Exp, Mod, Acc) ->
    NewAcc =
        case Exp band 1 of
            1 -> mul_mod(Acc, Base, Mod);
            0 -> Acc
        end,
    pow_mod_iter(mul_mod(Base, Base, Mod), Exp bsr 1, Mod, NewAcc).

mul_mod(A, B, Mod) ->
    A1 = A rem Mod,
    B1 = B rem Mod,
    {Smaller, Larger} =
        if
            A1 < B1 -> {A1, B1};
            true -> {B1, A1}
        end,
    mul_mod_iter(Smaller, Larger, Mod, 0).

mul_mod_iter(0, _B, _Mod, Result) ->
    Result;
mul_mod_iter(A, B, Mod, Result) ->
    NewResult =
        case A band 1 of
            1 -> add_mod(Result, B, Mod);
            0 -> Result
        end,
    mul_mod_iter(A bsr 1, add_mod(B, B, Mod), Mod, NewResult).

add_mod(A, B, Mod) ->
    case A > ?MAX_256 - B of
        true ->
            A - (Mod - B);
        false ->
            Sum = A + B,
            case Sum >= Mod of
                true -> Sum - Mod;
                false -> Sum
            end
    end.

mod_inverse(A, M) ->
    mod_inverse_iter(A rem M, M, 0, 1, M).

mod_inverse_iter(0, B, _, _, _) when B > 1 ->
    error(no_inverse);
mod_inverse_iter(0, _, _, V, M) ->
    (V rem M + M) rem M;
mod_inverse_iter(A, B, U, V, M) ->
    Q = B div A,
    mod_inverse_iter(B - Q * A, A, V - Q * U, U, M).

% linear congruential generator: arithmetical test with X(n+1) = (a * X(n) + c) mod m
test_lcg() ->
    % 128-bit modulus with 64-bit multiplier
    M1 = ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD),
    A1 = ?MODULE:id(6364136223846793005),
    C1 = ?MODULE:id(1442695040888963407),
    Seed1 = ?MODULE:id(16#DEADBEEFCAFEFACEDEADBEEFCAFEFACE),

    Seq1 = lcg_generate_list(Seed1, A1, C1, M1, 10),

    [
        284412277832923911036252819523197902945,
        242429421340901567576096743226374302589,
        129120757381660263416326020558477411268,
        101374252394522742917368046220189002955,
        275130720095757094664357552001296282437,
        305917752818921814664933056477789646635,
        160978969644231458610552848751498156998,
        83042210244989235895948247588550999002,
        110925512882941274022787798912050376922,
        57671813172898767662114567250138138025
    ] = Seq1,

    Seq1 = lcg2_generate_list(Seed1, A1, C1, M1, 10),

    % 200-bit modulus with small multiplier
    M2 = ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),
    A2 = 137,
    C2 = 17,
    Seed2 = ?MODULE:id(16#CAFEFACECAFEFACECAFEFACECAFEFACECAFEFACECAFEFACE),

    Seq2 = lcg_generate_list(Seed2, A2, C2, M2, 10),
    [
        681910347657827383559099367180830884836131950446222861678671,
        219311062100915566162811947986400276262303571730547602498194,
        1120730711163607604549919211995911002535935438994750506827845,
        880993224810165646852533271029360107813870732915500081784157,
        175718479568422953149901205435139581335065875714048556826401,
        1576299081248080723948995851837846207593183059865552590997704,
        623276200282362258389511328069141702290878034686465036301215,
        221123092957144795639061051390795280173532081557689702293597,
        1368978938467012042796046378398026538374241285313218178798056,
        1145301435937777900190751128954773864695508809106921600374189
    ] = Seq2,

    Seq2 = lcg2_generate_list(Seed2, A2, C2, M2, 10),

    % 127-bit modulus with 112-bit multiplier
    M3 = ?MODULE:id(16#7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),
    A3 = ?MODULE:id(16#7FFFFFFFFFFFFFFFFFFFFFFFFFFF),
    C3 = 1,
    Seed3 = ?MODULE:id(16#1234567890ABCDEF1234567890ABCDEF),

    Seq3 = lcg_generate_list(Seed3, A3, C3, M3, 10),
    [
        112668861072396317750088639033193054909,
        16758645762257078945430728044245353232,
        155418173783398464656932055970120463071,
        5999726299433229089746335152179713841,
        9413698473685442510902426879319860675,
        143289299629033878574739455745366823509,
        39037794831185278974748495794528352089,
        40282923797698809501466362282363762332,
        81082437642331301335362549162833267367,
        53604383939445517578052077790681050784
    ] = Seq3,

    Seq3 = lcg2_generate_list(Seed3, A3, C3, M3, 10),

    % Small parameters for verification

    % 2^16 + 1 (prime)
    MSmall = 65537,
    ASmall = 75,
    CSmall = 74,
    SeedSmall = 12345,

    SeqSmall = lcg_generate_list(SeedSmall, ASmall, CSmall, MSmall, 5),
    [8431, 42566, 46748, 32713, 28680] = SeqSmall,

    SeqSmall = lcg2_generate_list(SeedSmall, ASmall, CSmall, MSmall, 5),

    % Test negative seed normalization
    NegSeed = -12345,
    SeqNeg = lcg_generate_list(NegSeed, ASmall, CSmall, MSmall, 3),
    [57254, 34219, 10556] = SeqNeg,

    SeqNeg = lcg2_generate_list(NegSeed, ASmall, CSmall, MSmall, 3),

    % Test zero seed
    SeqZero = lcg_generate_list(0, ASmall, CSmall, MSmall, 3),
    [74, 5624, 28652] = SeqZero,

    SeqZero = lcg2_generate_list(0, ASmall, CSmall, MSmall, 3),

    0.

lcg_next(X, A, C, M) ->
    Result = ((A * X) + C) rem M,
    % Ensure positive result (rem can return negative for negative X)
    case Result < 0 of
        true -> Result + M;
        false -> Result
    end.

lcg_generate_list(_Seed, _A, _C, _M, 0) ->
    [];
lcg_generate_list(Seed, A, C, M, N) ->
    Next = lcg_next(Seed, A, C, M),
    [Next | lcg_generate_list(Next, A, C, M, N - 1)].

% LCG using subtraction and negative operations
% Formula: (A*X + C) becomes (A*X - (-C))
lcg2_next(X, A, C, M) ->
    Product = A * X,
    NegC = ?MODULE:id(-C),
    MaskedNegC = ?MODULE:id(?MODULE:id(-1) band NegC),
    Result = (Product - MaskedNegC) rem M,

    % Ensure positive result
    case Result < 0 of
        true -> Result + M;
        false -> Result
    end.

lcg2_generate_list(_Seed, _A, _C, _M, 0) ->
    [];
lcg2_generate_list(Seed, A, C, M, N) ->
    Next = lcg2_next(Seed, A, C, M),
    [Next | lcg2_generate_list(Next, A, C, M, N - 1)].

% population count: tests band and bsr operations
test_count_pop() ->
    130 = count_pop(
        ?MODULE:id(16#7FC2802D66FFBD055BB36A81274E4D83E9351A542884517AEA7516FF6643A4BD)
    ),
    133 = count_pop(
        ?MODULE:id(16#9A326A408959DFE6D4418677B2EA4CF28F66C33470FAEF07381BB22AF4F8FB69)
    ),
    130 = count_pop(
        ?MODULE:id(-16#243EAE0A4B6AB48A4671AAF9AE341F71DEC9E1DA3232F263E803E1C241ADD34D)
    ),
    126 = count_pop(
        ?MODULE:id(-16#6AFDB39E08B967B10121895DA7A435D8B4157E512251FFE6D43295C249FE91B7)
    ),
    66 = count_pop(?MODULE:id(16#34157E512251FFE6D43295C249FE91B7)),
    256 = count_pop(
        ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
    ),
    128 = count_pop(
        ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000)
    ),
    4 = count_pop(-?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)),
    130 = count_pop(
        bnot (?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000))
    ),

    130 = count_pop2(
        ?MODULE:id(16#7FC2802D66FFBD055BB36A81274E4D83E9351A542884517AEA7516FF6643A4BD)
    ),
    133 = count_pop2(
        ?MODULE:id(16#9A326A408959DFE6D4418677B2EA4CF28F66C33470FAEF07381BB22AF4F8FB69)
    ),
    131 = count_pop2(
        ?MODULE:id(-16#243EAE0A4B6AB48A4671AAF9AE341F71DEC9E1DA3232F263E803E1C241ADD34D)
    ),
    127 = count_pop2(
        ?MODULE:id(-16#6AFDB39E08B967B10121895DA7A435D8B4157E512251FFE6D43295C249FE91B7)
    ),
    66 = count_pop2(?MODULE:id(16#34157E512251FFE6D43295C249FE91B7)),
    256 = count_pop(
        ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
    ),
    128 = count_pop2(
        ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000)
    ),
    2 = count_pop2(
        -?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
    ),
    130 = count_pop(
        bnot (?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000))
    ),

    0.

count_pop(N) when is_integer(N) ->
    count_pop(N, 0).

count_pop(-1, Acc) ->
    Acc;
count_pop(0, Acc) ->
    Acc;
count_pop(N, Acc) ->
    Pop =
        case N band 7 of
            2#000 -> 0;
            2#001 -> 1;
            2#010 -> 1;
            2#011 -> 2;
            2#100 -> 1;
            2#101 -> 2;
            2#110 -> 2;
            2#111 -> 3
        end,
    count_pop(N bsr 3, Acc + Pop).

count_pop2(N) when is_integer(N) ->
    count_pop2(N, 1, 0).

count_pop2(N, 16#8000000000000000000000000000000000000000000000000000000000000000, Pop) ->
    case N band 16#8000000000000000000000000000000000000000000000000000000000000000 =/= 0 of
        true -> Pop + 1;
        false -> Pop
    end;
count_pop2(N, Shifted, Pop) when Shifted > 0 ->
    case N band Shifted of
        0 -> count_pop2(N, Shifted bsl 1, Pop);
        _N -> count_pop2(N, Shifted bsl 1, Pop + 1)
    end.

% bloom filter: tests bsr, bor and band operations
test_bloom() ->
    EmptyFilter = 0,

    TrainingWords = ?MODULE:id([
        % "The quick brown fox jumps over the lazy dog"
        <<"the">>,
        <<"quick">>,
        <<"brown">>,
        <<"fox">>,
        <<"jumps">>,
        <<"over">>,
        <<"lazy">>,
        <<"dog">>,
        % Lorem ipsum words
        <<"lorem">>,
        <<"ipsum">>,
        <<"dolor">>,
        <<"sit">>,
        <<"amet">>,
        <<"consectetur">>,
        <<"adipiscing">>,
        <<"elit">>,
        <<"sed">>,
        <<"do">>,
        <<"eiusmod">>,
        <<"tempor">>,
        <<"incididunt">>,
        % Some additional common words
        <<"and">>,
        <<"or">>,
        <<"but">>,
        <<"with">>,
        <<"from">>
    ]),

    Filter = bloom_add_all(TrainingWords, EmptyFilter),

    % Verify the filter is using big integers
    true = (Filter > 16#FFFFFFFFFFFFFFFF),

    % Words that should be found (from training set)
    true = bloom_contains(<<"the">>, Filter),
    true = bloom_contains(<<"quick">>, Filter),
    true = bloom_contains(<<"fox">>, Filter),
    true = bloom_contains(<<"lorem">>, Filter),
    true = bloom_contains(<<"ipsum">>, Filter),
    true = bloom_contains(<<"and">>, Filter),
    true = bloom_contains(<<"lazy">>, Filter),
    true = bloom_contains(<<"dog">>, Filter),
    true = bloom_contains(<<"brown">>, Filter),
    true = bloom_contains(<<"jumps">>, Filter),
    true = bloom_contains(<<"over">>, Filter),
    true = bloom_contains(<<"dolor">>, Filter),
    true = bloom_contains(<<"sit">>, Filter),
    true = bloom_contains(<<"amet">>, Filter),
    true = bloom_contains(<<"consectetur">>, Filter),
    true = bloom_contains(<<"adipiscing">>, Filter),
    true = bloom_contains(<<"elit">>, Filter),
    true = bloom_contains(<<"sed">>, Filter),
    true = bloom_contains(<<"do">>, Filter),
    true = bloom_contains(<<"eiusmod">>, Filter),
    true = bloom_contains(<<"tempor">>, Filter),
    true = bloom_contains(<<"incididunt">>, Filter),
    true = bloom_contains(<<"or">>, Filter),
    true = bloom_contains(<<"but">>, Filter),
    true = bloom_contains(<<"with">>, Filter),
    true = bloom_contains(<<"from">>, Filter),

    % Words that should NOT be found
    false = bloom_contains(<<"yellow">>, Filter),
    false = bloom_contains(<<"was">>, Filter),
    false = bloom_contains(<<"caught">>, Filter),
    false = bloom_contains(<<"slow">>, Filter),
    false = bloom_contains(<<"red">>, Filter),
    false = bloom_contains(<<"sleeping">>, Filter),
    % false positive
    true = bloom_contains(<<"sun">>, Filter),

    % "Hello" in different languages - should not be found
    false = bloom_contains(<<"hello">>, Filter),
    false = bloom_contains(<<"bonjour">>, Filter),
    false = bloom_contains(<<"hola">>, Filter),
    false = bloom_contains(<<"ciao">>, Filter),
    false = bloom_contains(<<"hallo">>, Filter),
    false = bloom_contains(<<"konnichiwa">>, Filter),
    false = bloom_contains(<<"namaste">>, Filter),
    false = bloom_contains(<<"salam">>, Filter),
    false = bloom_contains(<<"nihao">>, Filter),

    % Empty binary shouldn't match (or very unlikely)
    false = bloom_contains(<<>>, Filter),

    16#5A0D0094109764006C0000202240485E04025A18504A820042300200B02086D4 = ?MODULE:id(Filter),

    % Not filter
    Filter2 = bnot (?MODULE:id(Filter)),
    -16#5A0D0094109764006C0000202240485E04025A18504A820042300200B02086D5 = ?MODULE:id(Filter2),

    false = bloom_contains(<<"the">>, Filter2),
    false = bloom_contains(<<"quick">>, Filter2),
    false = bloom_contains(<<"fox">>, Filter2),
    false = bloom_contains(<<"lorem">>, Filter2),
    false = bloom_contains(<<"ipsum">>, Filter2),
    false = bloom_contains(<<"and">>, Filter2),
    false = bloom_contains(<<"lazy">>, Filter2),
    false = bloom_contains(<<"dog">>, Filter2),
    false = bloom_contains(<<"brown">>, Filter2),
    false = bloom_contains(<<"jumps">>, Filter2),
    false = bloom_contains(<<"over">>, Filter2),
    false = bloom_contains(<<"dolor">>, Filter2),

    false = bloom_contains(<<"hello">>, Filter2),
    true = bloom_contains(<<"bonjour">>, Filter2),
    false = bloom_contains(<<"hola">>, Filter2),
    false = bloom_contains(<<"ciao">>, Filter2),

    % Test with maximum value filter (all bits set)
    MaxFilter = ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),
    % Everything matches when all bits are set
    true = bloom_contains(<<"anything">>, MaxFilter),
    true = bloom_contains(<<"xyz">>, MaxFilter),

    % Verify bit operations work correctly
    FilterWithOneBit = bloom_add(<<"test">>, 0),
    true = (FilterWithOneBit > 0),
    true = bloom_contains(<<"test">>, FilterWithOneBit),

    0.

bloom_add_all([], Filter) ->
    Filter;
bloom_add_all([Word | Rest], Filter) ->
    NewFilter = bloom_add(Word, Filter),
    bloom_add_all(Rest, NewFilter).

bloom_add(Binary, Filter) ->
    Pos1 = hash_position(Binary, 1),
    Pos2 = hash_position(Binary, 2),
    Pos3 = hash_position(Binary, 3),

    Filter1 = Filter bor (1 bsl Pos1),
    Filter2 = Filter1 bor (1 bsl Pos2),
    Filter3 = Filter2 bor (1 bsl Pos3),

    Filter3.

bloom_contains(Binary, Filter) ->
    Pos1 = hash_position(Binary, 1),
    Pos2 = hash_position(Binary, 2),
    Pos3 = hash_position(Binary, 3),

    Bit1 = (Filter bsr Pos1) band 1,
    Bit2 = (Filter bsr Pos2) band 1,
    Bit3 = (Filter bsr Pos3) band 1,

    (Bit1 =:= 1) andalso (Bit2 =:= 1) andalso (Bit3 =:= 1).

hash_position(Binary, Salt) ->
    hash_binary(Binary, Salt, 0, 0) rem 256.

hash_binary(<<>>, Salt, _Pos, Acc) ->
    ((Acc bxor Salt) * 2654435761) band 16#FFFFFFFF;
hash_binary(<<Byte:8, Rest/binary>>, Salt, Pos, Acc) ->
    Rotated = ((Acc bsl 5) bor (Acc bsr 27)) band 16#FFFFFFFF,
    Mixed = (Rotated bxor Byte) * (16#9E3779B1 + (Salt * 16#85EBCA6B)),
    hash_binary(Rest, Salt, Pos + 1, (Mixed + Pos) band 16#FFFFFFFF).

% xorshift256 pseudo random: tests bxor, band and bsr operations
test_xorshift256() ->
    16#BBDC13EB8C1FE01136E699C6325EC4001F5B57AB0E0FD01BD5AA42368F525E01 = ?MODULE:id(
        times(
            fun(X) -> xorshift256(X) end,
            ?MODULE:id(16#F02322ED0F1AB7C5888870265271B6EB9C691C764DC12024C53D780858EF6D5F),
            300
        )
    ),
    16#BAEFDAB2E9326D6709BF57A53AF1C90D2F257DD9F3CAEE0512CD6A61E5918D87 = ?MODULE:id(
        times(
            fun(X) -> xorshift256(X) end,
            ?MODULE:id(-16#E02322ED0F1AB7C5888870265271B6AB9C691C764DC11024C53D780858EF6D55),
            250
        )
    ),
    16#34412991549EF0BDD10255CD9B3D9D60A928EA5EC03B28F99DD3CEAFC17EEF0D = ?MODULE:id(
        times(
            fun(X) -> xorshift256(X) end,
            ?MODULE:id(16#FFFFFFFFAAAABBBB),
            250
        )
    ),

    0.

xorshift256(State) when is_integer(State) andalso State =/= 0 ->
    S1 = State bxor safe_bsl(State, 12),
    S2 = S1 bxor (S1 bsr 25),
    S2 bxor safe_bsl(S2, 27).

safe_bsl(_N, Shift) when Shift >= 256 ->
    0;
safe_bsl(N, Shift) when Shift < 256 ->
    (N band (?MAX_256 bsr Shift)) bsl Shift.

% galois field math: tests bxor and band operations
test_gf256() ->
    16#6666666666666666666666666666666666666666666666666666666666666666 = multiply(
        16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF, 16#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    ),

    16#11000000110000001100000011 = power(3, 100),
    16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF = power(3, 127),
    16#101000000000000010100000000000001010000000000000101 = power(3, 200),
    16#43A2E143A2E143A2E143A2E143A2E143A2E143A2E143A2E143A2E143A2E143F4 = power(7, 250),
    16#424001100340435002504010410042400110034043500250401041004240001 = power(11, 240),
    16#3FF83230E915D6EDE4DD0DC8DB253FF83230E915D6EDE4DD0DC8DB253FF832C8 = power(13, 245),

    16#3E7C7CF8F9F1F3E3E7C7CF8F9F1F3E3E7CF8F9F1F3E3E7C7CF8F9F1F3E3E7C82 = inverse(
        16#80000000000000000000000000000001
    ),
    16#E05A98F90F355292A3134DF1056F4EEA0000000000000000000000000000039E = inverse(1 bsl 127),

    16#176C0C6F176C0C6F176C0C6F176C0C6F176C0C6F176C0C6F176C0C6F176C0C30 = inverse(16#FFFFFFFF),
    16#D16BC03173798DB5A2D78062E6F31B6B45AF00C5CDE636D68B5E018B9BCC6EF1 = inverse(
        16#7FFFFFFFFFFFFFFF
    ),
    16#D19D76C0C6F17AAFD19D76C0C6F17AAFD19D76C0C6F17AAFD19D76C0C6F179F0 = inverse(
        16#FFFFFFFFFFFFFFFF
    ),
    16#1FB1AE70D3B9D7FA8FD8D73869DCEBFD47EC6B9C34EE75FEA3F635CE1A773A82 = inverse(
        16#1FFFFFFFFFFFFFFFF
    ),
    16#20E7D9119D76C0C6F17AAFD15B87BA6920E7D9119D76C0C6F17AAFD15B87BAEE = inverse(
        16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    ),
    16#8F368296271FE0E7D9119D76C0C6F17AAFD15B87BA692021286B32A79B41493C = inverse(
        16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    ),

    0.

multiply(A, B) ->
    multiply_iter(A, B, 0).

multiply_iter(0, _B, Result) ->
    Result;
multiply_iter(A, B, Result) ->
    NewResult =
        case A band 1 of
            1 -> Result bxor B;
            0 -> Result
        end,

    NeedReduction = (B bsr 255) band 1,

    B_shifted = safe_bsl(B, 1),

    B_next =
        case NeedReduction of
            1 -> B_shifted bxor ((1 bsl 10) bor (1 bsl 5) bor (1 bsl 2) bor 1);
            0 -> B_shifted
        end,

    multiply_iter(A bsr 1, B_next, NewResult).

power(_A, 0) ->
    1;
power(A, 1) ->
    A;
power(A, N) when N > 0 ->
    power_iter(A, N, 1).

power_iter(_A, 0, Result) ->
    Result;
power_iter(A, N, Result) ->
    NewResult =
        case N band 1 of
            1 -> multiply(Result, A);
            0 -> Result
        end,
    A_squared = multiply(A, A),
    power_iter(A_squared, N bsr 1, NewResult).

inverse(0) ->
    error(zero_has_no_inverse);
inverse(A) ->
    Exponent = (1 bsl 256) - 2,
    power(A, Exponent).

% UUID classification: tests pattern matching with big integers
test_uuid() ->
    UUIDv1 = ?MODULE:id(16#51C1CD6EADB611F09C35325096B39F47),
    {valid, v1, UUIDv1} = classify_uuid_pattern(UUIDv1),

    UUIDv2 = ?MODULE:id(16#000003E8ADB721F09F00325096B39F47),
    {valid, v2, UUIDv2} = classify_uuid_pattern(UUIDv2),

    UUIDv3 = ?MODULE:id(16#4738BDFB25A3829A801B21A1D25095B),
    {valid, v3, UUIDv3} = classify_uuid_pattern(UUIDv3),

    UUIDv4 = ?MODULE:id(16#8D8AC610566D4EF09C22186B2A5ED793),
    {valid, v4, UUIDv4} = classify_uuid_pattern(UUIDv4),

    UUIDv5 = ?MODULE:id(16#CFBFF0D193755685968C48CE8B15AE17),
    {valid, v5, UUIDv5} = classify_uuid_pattern(UUIDv5),

    UUIDv6 = ?MODULE:id(16#1F0ADB874AB06C808B0F4120893F2871),
    {valid, v6, UUIDv6} = classify_uuid_pattern(UUIDv6),

    UUIDv7 = ?MODULE:id(16#019A01CED63A700CA7CDB76C92F0C7BB),
    {valid, v7, UUIDv7} = classify_uuid_pattern(UUIDv7),

    UUIDv8 = ?MODULE:id(16#F31ED01D32FA8CACAFDA019A01D1EFB8),
    {valid, v8, 16#A, UUIDv8} = classify_uuid_pattern(UUIDv8),

    InvalidVersion = ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1),
    {invalid, version} = classify_uuid_pattern(InvalidVersion),

    NilUUID = ?MODULE:id(0),
    {valid, nil} = classify_uuid_pattern(NilUUID),

    MaxUUID = ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),
    {valid, max} = classify_uuid_pattern(MaxUUID),

    LargeValue = ?MODULE:id(16#10000000000000000000000000000000000),
    {invalid, not_uuid} = classify_uuid_pattern(LargeValue),

    Negative = ?MODULE:id(-1),
    {invalid, not_uuid} = classify_uuid_pattern(Negative),

    0.

classify_uuid_pattern(UUID) when
    is_integer(UUID) andalso UUID >= 0 andalso UUID =< 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
->
    case UUID band 16#F0000000000000000000 of
        0 ->
            {valid, nil};
        16#10000000000000000000 ->
            {valid, v1, UUID};
        16#20000000000000000000 ->
            {valid, v2, UUID};
        16#30000000000000000000 ->
            {valid, v3, UUID};
        16#40000000000000000000 ->
            {valid, v4, UUID};
        16#50000000000000000000 ->
            {valid, v5, UUID};
        16#60000000000000000000 ->
            {valid, v6, UUID};
        16#70000000000000000000 ->
            {valid, v7, UUID};
        16#80000000000000000000 ->
            case UUID band 16#F000000000000000 of
                16#8000000000000000 -> {valid, v8, 8, UUID};
                16#9000000000000000 -> {valid, v8, 9, UUID};
                16#A000000000000000 -> {valid, v8, 16#A, UUID};
                16#B000000000000000 -> {valid, v8, 16#B, UUID};
                _ -> {invalid, variant}
            end;
        16#F0000000000000000000 when UUID == 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF -> {valid, max};
        _ ->
            {invalid, version}
    end;
classify_uuid_pattern(NotUUID) when is_integer(NotUUID) ->
    {invalid, not_uuid};
classify_uuid_pattern(_) ->
    {invalid, not_integer}.

% bit rotation: tests bsl, band and bor operations
test_bit_rotation() ->
    % Simple patterns with known results
    Pattern1 = ?MODULE:id(16#0000000000000000000000000000000000000000000000000000000000000001),

    % Rotate left by 1
    R1_L1 = rotate_left(Pattern1, 1, 256),
    16#0000000000000000000000000000000000000000000000000000000000000002 = R1_L1,

    % Rotate left by 8
    R1_L8 = rotate_left(Pattern1, 8, 256),
    16#0000000000000000000000000000000000000000000000000000000000000100 = R1_L8,

    % Rotate left by 255 (should wrap to MSB)
    R1_L255 = rotate_left(Pattern1, 255, 256),
    16#8000000000000000000000000000000000000000000000000000000000000000 = R1_L255,

    % Pattern with multiple bits set
    Pattern2 = ?MODULE:id(16#DEADBEEFCAFEFACE1234567890ABCDEF0123456789ABCDEF0123456789ABCDEF),

    % Rotate left by 4 (one hex digit)
    R2_L4 = rotate_left(Pattern2, 4, 256),
    16#EADBEEFCAFEFACE1234567890ABCDEF0123456789ABCDEF0123456789ABCDEFD = R2_L4,

    % Rotate right by 4
    R2_R4 = rotate_right(Pattern2, 4, 256),
    16#FDEADBEEFCAFEFACE1234567890ABCDEF0123456789ABCDEF0123456789ABCDE = R2_R4,

    % Verify rotation is reversible
    Pattern2 = rotate_right(rotate_left(Pattern2, 17, 256), 17, 256),
    Pattern2 = rotate_left(rotate_right(Pattern2, 63, 256), 63, 256),

    % Maximum value (all bits set)
    MaxPattern = ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),

    % Rotating all 1s should still be all 1s
    MaxPattern = rotate_left(MaxPattern, 1, 256),
    MaxPattern = rotate_left(MaxPattern, 128, 256),
    MaxPattern = rotate_right(MaxPattern, 77, 256),

    % Half-filled pattern
    HalfPattern = ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000),

    % Rotate by 128 bits (swap halves)
    R4_L128 = rotate_left(HalfPattern, 128, 256),
    16#00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF = R4_L128,

    % Byte-aligned rotations
    BytePattern = ?MODULE:id(16#FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00),

    R5_L8 = rotate_left(BytePattern, 8, 256),
    16#00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF = R5_L8,

    R5_R8 = rotate_right(BytePattern, 8, 256),
    16#00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF = R5_R8,

    % Edge cases
    % Rotate by 0 (no change)
    Pattern2 = rotate_left(Pattern2, 0, 256),
    Pattern2 = rotate_right(Pattern2, 0, 256),

    % Rotate by 256 (full rotation, no change)
    Pattern2 = rotate_left(Pattern2, 256, 256),
    Pattern2 = rotate_right(Pattern2, 256, 256),

    % Rotate by more than 256 (should wrap)
    true = (rotate_left(Pattern1, 1, 256) =:= rotate_left(Pattern1, 257, 256)),
    true = (rotate_left(Pattern1, 10, 256) =:= rotate_left(Pattern1, 266, 256)),

    % Small patterns in 256-bit space
    SmallPattern = ?MODULE:id(16#CAFE),

    R7_L100 = rotate_left(SmallPattern, 100, 256),
    65874731091460169078177678770569216 = R7_L100,

    % Alternating bit pattern
    AltPattern = ?MODULE:id(16#5555555555555555555555555555555555555555555555555555555555555555),

    R8_L1 = rotate_left(AltPattern, 1, 256),
    16#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA = R8_L1,

    R8_R1 = rotate_right(AltPattern, 1, 256),
    16#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA = R8_R1,

    % Single bit moving through all positions
    SingleBit = 1,
    verify_single_bit_rotation(SingleBit, 0, 256),

    RandomPattern = ?MODULE:id(16#2AFCC6CA43CA5D5C862F0E2AE5DCC5EAE299E01E5AC2FDE8974E09A0190),
    Rand_R11 = rotate_left(RandomPattern, 11, 255),
    16#157E636521E52EAE4317871572EE62F5714CF00F2D617EF44BA704D00C8000 = Rand_R11,

    0.

rotate_left(Value, 0, _Width) ->
    Value;
rotate_left(Value, Shift, 256) ->
    NormalizedShift = Shift rem 256,
    case NormalizedShift of
        0 ->
            Value;
        _ ->
            MaxVal = 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF,
            MaskedValue = Value band MaxVal,

            LowerMask = (1 bsl (256 - NormalizedShift)) - 1,
            LowerBits = MaskedValue band LowerMask,
            ShiftedLower = LowerBits bsl NormalizedShift,

            UpperBits = MaskedValue bsr (256 - NormalizedShift),

            ShiftedLower bor UpperBits
    end;
rotate_left(Value, Shift, Width) ->
    NormalizedShift = Shift rem Width,
    case NormalizedShift of
        0 ->
            Value;
        _ ->
            Mask = (1 bsl Width) - 1,
            MaskedValue = Value band Mask,
            Left = (MaskedValue bsl NormalizedShift) band Mask,
            Right = MaskedValue bsr (Width - NormalizedShift),
            Left bor Right
    end.

rotate_right(Value, 0, _Width) ->
    Value;
rotate_right(Value, Shift, 256) ->
    NormalizedShift = Shift rem 256,
    case NormalizedShift of
        0 ->
            Value;
        _ ->
            MaxVal = 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF,
            MaskedValue = Value band MaxVal,

            ShiftedRight = MaskedValue bsr NormalizedShift,

            BottomMask = (1 bsl NormalizedShift) - 1,
            BottomBits = MaskedValue band BottomMask,
            case 256 - NormalizedShift of
                ShiftAmount when ShiftAmount < 256 ->
                    MovedBits = BottomBits bsl ShiftAmount,
                    ShiftedRight bor MovedBits;
                _ ->
                    ShiftedRight
            end
    end;
rotate_right(Value, Shift, Width) ->
    NormalizedShift = Shift rem Width,
    case NormalizedShift of
        0 ->
            Value;
        _ ->
            Mask = (1 bsl Width) - 1,
            MaskedValue = Value band Mask,
            Right = MaskedValue bsr NormalizedShift,
            BottomMask = (1 bsl NormalizedShift) - 1,
            BottomBits = MaskedValue band BottomMask,
            Left = BottomBits bsl (Width - NormalizedShift),
            (Left bor Right) band Mask
    end.

verify_single_bit_rotation(_Value, 256, _Width) ->
    ok;
verify_single_bit_rotation(Value, Position, Width) ->
    Expected = 1 bsl Position,
    Rotated = rotate_left(1, Position, Width),
    ExpectedMasked =
        case Width of
            256 ->
                case Position < 256 of
                    true -> Expected;
                    false -> 1 bsl (Position rem 256)
                end;
            _ ->
                Expected band ((1 bsl Width) - 1)
        end,
    case Rotated =:= ExpectedMasked of
        true -> verify_single_bit_rotation(Value, Position + 1, Width);
        false -> error({rotation_mismatch, Position, Expected, Rotated})
    end.

% gray code with not: tests bnot, band and bor operations
% Implements XOR using De Morgan's law: A XOR B = (A AND (NOT B)) OR ((NOT A) AND B)
test_gray_code_with_not() ->
    % Small values with known Gray codes
    0 = to_gray_with_not(0),
    1 = to_gray_with_not(1),
    3 = to_gray_with_not(2),
    2 = to_gray_with_not(3),
    6 = to_gray_with_not(4),
    7 = to_gray_with_not(5),
    5 = to_gray_with_not(6),
    4 = to_gray_with_not(7),

    % Verify with larger small values
    12 = to_gray_with_not(8),
    24 = to_gray_with_not(16),
    120 = to_gray_with_not(80),

    % Big integer Gray codes
    Big1 = ?MODULE:id(16#CAFEFACECAFEFACECAFEFACECAFEFACE),
    Gray1 = to_gray_with_not(Big1),
    16#AF8187A9AF8187A9AF8187A9AF8187A9 = Gray1,

    Big2 = ?MODULE:id(16#123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0),
    Gray2 = to_gray_with_not(Big2),
    16#1B2E7D44D7E2B1881B2E7D44D7E2B1881B2E7D44D7E2B1881B2E7D44D7E2B188 = Gray2,

    Big3 = ?MODULE:id(16#D465BFE75338F6739A8FA57BB16B26),
    Gray3 = to_gray_with_not(Big3),
    16#BE576014FAA48D4A57C877C669DEB5 = Gray3,

    Big4 = ?MODULE:id(16#2D652C60062E730EF673D107671CD3),
    Gray4 = to_gray_with_not(Big4),
    16#3BD7BA5005394A898D4A3984D492BA = Gray4,

    Big5 = ?MODULE:id(16#6E29C73A5E02E9D3A2323AC735028E258AC50291CAE1ED4359E87BD6F81821F2),
    Gray5 = to_gray_with_not(Big5),
    16#593D24A771039D3A732B27A4AF83C9374FA783D92F911BE2F51C463D8414310B = Gray5,

    Big6 = ?MODULE:id(16#1D1D5DC3927446CED87456278DCB77C9C7F2BC7E29A92DC03F30D),
    Gray6 = to_gray_with_not(Big6),
    16#1393F3225B4E65A9B44E7D344B2ECC2D240BE2413D7DBB2020A8B = Gray6,

    Big7 = ?MODULE:id(16#A4E9AB4A2507E9635ED3B5BC92E2DBEA),
    Gray7 = to_gray_with_not(Big7),
    16#F69D7EEF37841DD2F1BA6F62DB93B61F = Gray7,

    % Patterns at bit boundaries
    Pattern64 = ?MODULE:id(16#FFFFFFFFFFFFFFFF),
    Gray64 = to_gray_with_not(Pattern64),
    % All 1s -> 1 followed by 0s in Gray
    16#8000000000000000 = Gray64,

    Pattern128 = ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),
    Gray128 = to_gray_with_not(Pattern128),
    16#80000000000000000000000000000000 = Gray128,

    % Test alternating patterns
    NotPattern1 = ?MODULE:id(16#5555555555555555555555555555555555555555555555555555555555555555),
    GrayNot1 = to_gray_with_not(NotPattern1),
    16#7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF = GrayNot1,

    NotPattern2 = ?MODULE:id(16#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA),
    GrayNot2 = to_gray_with_not(NotPattern2),
    16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF = GrayNot2,

    % Maximum value edge case
    MaxVal = ?MODULE:id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),
    GrayMax = to_gray_with_not(MaxVal),
    16#7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF = GrayMax,

    % Test multiple NOT operations in sequence
    TestVal = ?MODULE:id(16#DEADBEEF),
    Gray = to_gray_with_not(TestVal),
    16#B1FB6198 = Gray,

    % Verify NOT operations work correctly with result
    NotGray = bnot Gray,
    XorResult = xor_using_not(Gray, NotGray),
    -1 = XorResult,

    % Zero and one edge cases with NOT
    0 = xor_using_not(0, 0),
    1 = xor_using_not(1, 0),
    1 = xor_using_not(0, 1),
    0 = xor_using_not(1, 1),

    % Test XOR properties using NOT
    A = ?MODULE:id(16#123456789ABCDEF),
    B = ?MODULE:id(16#FEDCBA987654321),

    % A XOR B = B XOR A (commutative)
    ResultAB = xor_using_not(A, B),
    ResultBA = xor_using_not(B, A),
    ResultAB = ResultBA,

    % A XOR 0 = A (identity)
    A = xor_using_not(A, 0),

    % A XOR A = 0 (self-inverse)
    0 = xor_using_not(A, A),

    0.

to_gray_with_not(Binary) ->
    Shifted = Binary bsr 1,
    xor_using_not(Binary, Shifted).

% A XOR B = (A AND (NOT B)) OR ((NOT A) AND B)
xor_using_not(A, B) ->
    case {A, B} of
        {0, 0} ->
            0;
        {0, _} ->
            B;
        {_, 0} ->
            A;
        _ ->
            NotB = bnot B,
            NotA = bnot A,

            Left = A band NotB,
            Right = NotA band B,

            % Special handling for max value case: bnot of max gives 0 in AtomVM.
            % Usually we do `case erlang:system_info(machine) of`,
            % but actually this is no-op on the BEAM (so it is not required).
            case {A, B} of
                {16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF, _} ->
                    B;
                {_, 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF} ->
                    A;
                _ ->
                    Left bor Right
            end
    end.

% general helpers
times(_Fun, A, 0) ->
    A;
times(Fun, A, N) ->
    A1 = Fun(A),
    times(Fun, A1, N - 1).

id(X) ->
    X.
