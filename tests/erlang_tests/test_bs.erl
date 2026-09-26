%
% This file is part of AtomVM.
%
% Copyright 2019-2021 Fred Dushin <fred@dushin.net>
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

-module(test_bs).

-export([start/0, ext_id/1, join/2]).

start() ->
    test_pack_small_ints({2, 61, 20}, <<23, 180>>),
    test_pack_integer_big_endian(1024, 32, <<0, 0, 4, 0>>),
    IntegersAndBinaries = test_pack_integers_and_binaries(
        16#F,
        16#2,
        <<"fubar">>,
        <<"Haddock's Eyes">>,
        <<0, 0, 0, 15, 0, 2, 102, 117, 98, 97, 114, 72, 97, 100>>
    ),
    test_unpack_integers_and_binaries(IntegersAndBinaries, 16#F, 16#2, <<"fubar">>, <<"Had">>),

    ok = test_create_with_invalid_int_value(),
    ok = test_create_with_invalid_int_size(),
    ok = test_create_with_int_unit(),
    ok = test_create_with_unaligned_int_size(),
    ok = test_bitstring_compare(),
    ok = test_bitstring_bif_guards(),
    ok = test_bitstring_segments(),
    ok = test_little_endian_unaligned(),
    ok = test_bs_match_string_trailing(),
    ok = test_dynamic_size_extraction(),
    ok = test_signed_int_unaligned(),
    ok = test_signed_int_zero_width(),
    ok = test_big_int_unaligned_unsupported(),
    ok = test_non_pow2_unit(),
    ok = test_create_with_int_little_endian(),
    ok = test_create_with_int_signed(),
    ok = test_create_with_invalid_binary_value(),
    ok = test_create_with_invalid_binary_size(),
    ok = test_create_with_binary_size_out_of_range(),
    ok = test_create_with_unsupported_binary_unit(),

    15 = get_integer_big_unsigned(<<16#F>>, 8),
    128 = get_integer_big_unsigned(<<16#80>>, 8),
    4404 = get_integer_big_unsigned(<<0, 0, 17, 52>>, 32),

    ok = test_get_with_unsupported_int_unit(),
    ok = test_get_with_int_little_endian(),
    ok = test_get_with_int_signed(),
    ok = test_get_with_unaligned_binary(),
    ok = test_large_unaligned_slice(),
    ok = test_private_append_bitstring(),

    <<"">> = test_match_first_integer(<<16#FF>>),
    <<1, 2, 3>> = test_match_first_integer(<<16#FF, 1, 2, 3>>),
    <<1, 2, 3>> = test_match_first_integer(<<16#AB, 16#CD, 1, 2, 3>>),
    nope = test_match_first_integer(<<16#00, 1, 2, 3>>),

    <<1, 2, 3, 1, 2, 3, 4, 5, 6>> = test_bs_append(<<1, 2, 3>>, <<1, 2, 3, 4, 5, 6>>),

    <<1, 2, 3>> = test_bs_private_append(<<1, 2, 3>>),

    % Large case sufficient to trigger a valgrind error if reused binary is not zero'd
    Expected = make_binary_copy(32, 0, <<>>),
    Expected = test_bs_private_append2(id(make_binary_copy(32, 240, <<>>)), <<>>),

    nope = test_match_clause(<<"">>),
    nope = test_match_clause(<<16#FF>>),
    nope = test_match_clause(<<$n:8>>),
    nope = test_match_clause(<<$n:8, 1, 2, 3>>),
    {$n, <<1, 2, 3, 4>>, <<"">>} = test_match_clause(<<$n:8, 1, 2, 3, 4>>),
    {$n, <<1, 2, 3, 4>>, <<5, 6>>} = test_match_clause(<<$n:8, 1, 2, 3, 4, 5, 6>>),

    [<<"">>] = test_match_recursive(<<"">>, []),
    [<<"">>, 119] = test_match_recursive(<<119:32>>, []),
    [<<"">>, 119, 122] = test_match_recursive(<<122:8, 119:32>>, []),
    [<<"">>, 122, 122, 119, 119, 119, 122] = test_match_recursive(
        <<122:8, 119:32, 119:32, 119:32, 122:8, 122:8>>,
        []
    ),
    nope = test_match_recursive(<<"foo">>, []),

    BigBin = make_binary(1025),
    FirstPart = binary:part(BigBin, 0, 1024),
    LastPart = binary:part(BigBin, 1024, 1),
    {FirstPart, LastPart} = test_match_force_gc(BigBin),

    test_put_match_string(<<"foo">>, <<"bar">>),
    test_skip_bits(),
    ok = test_bs_match_string_unaligned(),

    test_match_case_type(),

    ok = test_iterate_binary(),

    ok = test_large(),

    ok = test_copy_bits_string(),
    ok = test_bs_match_string_select(),

    ok = test_bs_skip_bits2_little(),

    ok = test_bs_variable_size_bitstring(),
    ok = test_negative_dynamic_size(),
    ok = test_oversized_dynamic_size(),
    ok = test_construction_size_overflow(),
    ok = test_wide_integer_construction(),
    ok = test_float(),

    0.

test_pack_small_ints({A, B, C}, Expect) ->
    Expect = <<A:5, B:6, C:5>>,
    Expect.

test_pack_integer_big_endian(Int, Size, Expect) ->
    Expect = <<Int:Size>>,
    Expect.

test_pack_integers_and_binaries(Int1, Int2, Bin1, Bin2, Expect) ->
    Bin = <<Int1:32/big, Int2:16, Bin1/binary, Bin2:3/binary>>,
    %% erlang:display(Bin),
    Expect = Bin,
    Expect.

test_unpack_integers_and_binaries(Bin, Int1, Int2, Bin1, Bin2) ->
    <<A:32, B:16, C:5/binary, D/binary>> = Bin,
    A = Int1,
    B = Int2,
    C = Bin1,
    D = Bin2,
    Bin.

test_create_with_invalid_int_value() ->
    expect_error(fun() -> create_int_binary(foo, id(32)) end, badarg).

test_create_with_invalid_int_size() ->
    expect_error(fun() -> create_int_binary(16#F, id(bar)) end, badarg).

test_create_with_int_unit() ->
    <<1, 66, 67>> = create_int_binary_unit_3(16#14243, id(8)),
    <<0, 0, 0, 1, 66, 67>> = create_int_binary_unit_3(16#14243, id(16)),
    <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15>> = create_int_binary_unit_3(16#F, id(32)),
    <<0, 0, 0, 0, 16#12, 16#34, 16#56, 16#78, 16#90, 16#AB, 16#CD, 16#EF>> = create_int_binary_unit_3(
        16#1234567890ABCDEF, id(32)
    ),
    ok.

test_create_with_unaligned_int_size() ->
    B = create_int_binary(16#FFFF, id(28)),
    28 = bit_size(B),
    4 = byte_size(B),
    false = is_binary(B),
    true = is_bitstring(B),
    ok.

test_bitstring_compare() ->
    false = id(<<5:31>>) =:= id(<<5:24>>),
    false = id(<<5:31>>) == id(<<5:24>>),
    true = id(<<5:3>>) =:= id(<<5:3>>),
    true = id(<<5:3>>) == id(<<5:3>>),
    true = id(<<(id(5)):31>>) =:= id(<<5:31>>),
    false = id(<<5:3>>) < id(<<4>>),
    true = id(<<4>>) < id(<<5:3>>),
    true = id(<<255>>) < id(<<255, 0:1>>),
    true = id(<<255, 0:1>>) < id(<<255, 1:1>>),
    true = id(<<1:1>>) > id(<<0:7>>),
    true = id(<<>>) < id(<<0:1>>),
    [<<>>, <<254>>, <<255>>, <<255, 0:1>>, <<255, 1:1>>] = sort_bitstrings(
        id([<<255, 1:1>>, <<255>>, <<255, 0:1>>, <<254>>, <<>>])
    ),
    ok.

% BIFs and NIFs that operate on binaries must badarg on a non-byte-aligned
% bitstring instead of silently truncating it to whole bytes; a few
% (byte_size/1, bit_size/1, size/1, split_binary/2) accept bitstrings.
test_bitstring_bif_guards() ->
    Bits9 = id(<<1:9>>),
    Bits1 = id(<<1:1>>),
    2 = byte_size(Bits9),
    9 = bit_size(Bits9),
    % size/1 rounds down to whole bytes, unlike byte_size/1 which rounds up
    1 = size(Bits9),
    0 = size(Bits1),
    % is_bitstring accepts a non-byte-aligned bitstring, is_binary does not
    true = erlang:is_bitstring(Bits9),
    false = erlang:is_binary(Bits9),
    true = erlang:is_bitstring(id(<<1, 2>>)),
    true = erlang:is_binary(id(<<1, 2>>)),
    false = erlang:is_bitstring(id({})),
    % binary_part/3 and binary:part/3 badarg on a non-byte-aligned bitstring
    % only since OTP 27; OTP 26 truncated to whole bytes. AtomVM follows the
    % modern behavior.
    HasBitstringPartGuard =
        erlang:system_info(machine) =:= "ATOM" orelse
            list_to_integer(erlang:system_info(otp_release)) >= 27,
    case HasBitstringPartGuard of
        true ->
            expect_error(fun() -> binary_part(Bits9, 0, 1) end, badarg),
            expect_error(fun() -> binary:part(Bits9, 0, 1) end, badarg);
        false ->
            ok
    end,
    % split_binary/2 keeps the trailing bits in the second part
    {<<>>, <<0, 1:1>>} = split_binary(Bits9, 0),
    {<<0>>, <<1:1>>} = split_binary(Bits9, 1),
    expect_error(fun() -> split_binary(Bits9, 2) end, badarg),
    expect_error(fun() -> iolist_to_binary(Bits1) end, badarg),
    expect_error(fun() -> list_to_binary([Bits1]) end, badarg),
    expect_error(fun() -> iolist_size(Bits1) end, badarg),
    expect_error(fun() -> binary_to_atom(Bits1, utf8) end, badarg),
    expect_error(fun() -> binary_to_list(Bits1) end, badarg),
    expect_error(fun() -> binary_to_term(Bits1) end, badarg),
    expect_error(fun() -> erlang:crc32(Bits1) end, badarg),
    expect_error(fun() -> binary:at(Bits9, 0) end, badarg),
    expect_error(fun() -> binary:copy(Bits1) end, badarg),
    expect_error(fun() -> binary:split(Bits9, id(<<0>>)) end, badarg),
    expect_error(fun() -> binary:first(Bits9) end, badarg),
    expect_error(fun() -> binary:last(Bits9) end, badarg),
    ok.

% A bitstring used as a segment source must be copied bit-granularly; these
% used to silently truncate the source to whole bytes. Expected values are
% written in byte layout so they build through the byte-aligned path even if
% the compiler does not constant-fold them.
test_bitstring_segments() ->
    Bits1 = id(<<1:1>>),
    <<1:1>> = copy_bitstring(Bits1),
    <<213, 1:1>> = append_to_bitstring(Bits1),
    <<5:3>> = copy_bitstring(id(<<5:3>>)),
    % explicit bit-sized bitstring segments
    <<2:3>> = take_bits(id(<<2:3>>)),
    <<5:4>> = sized_then_bit(id(<<2:3>>)),
    <<T2:2/bits, _/bits>> = id(<<5:3>>),
    <<2:2>> = T2,
    % mixed bitstring segments and a byte segment at an unaligned offset
    <<13:4>> = mix_bitstrings(Bits1, id(<<5:3>>)),
    <<255, 255, 1:1>> = sandwich(Bits1),
    ok.

copy_bitstring(B) -> <<B/bitstring>>.
append_to_bitstring(B) -> <<B/bitstring, 16#AB:8>>.
take_bits(B) -> <<B:3/bitstring>>.
sized_then_bit(B) -> <<B:3/bitstring, 1:1>>.
mix_bitstrings(P, Q) -> <<P/bits, Q/bits>>.
sandwich(P) -> <<255, P/bits, 255>>.

% Little-endian integers whose width is not a multiple of 8 lay out complete
% low-order bytes first, then the remaining high-order bits (OTP layout).
% Expected values are written in byte layout (verified on OTP) so they do not
% depend on the little-endian runtime path under test.
test_little_endian_unaligned() ->
    ok = check_le_cases([
        {1, 1, <<1:1>>, <<11:4>>},
        {4, 16#A, <<10:4>>, <<90:7>>},
        {7, 16#55, <<85:7>>, <<181, 1:2>>},
        {9, 16#155, <<85, 1:1>>, <<170, 11:4>>},
        {12, 16#ABC, <<188, 10:4>>, <<183, 74:7>>},
        {15, 16#5A5A, <<90, 90:7>>, <<171, 86, 2:2>>},
        {63, 16#123456789ABCDEF, <<239, 205, 171, 137, 103, 69, 35, 1:7>>,
            <<189, 249, 181, 113, 44, 232, 164, 96, 1:2>>}
    ]),
    % signed little-endian round trip of a negative value
    <<251, 15:4>> = make_le(id(-5), id(12)),
    <<S:12/little-signed>> = id(<<251, 15:4>>),
    -5 = S,
    ok.

check_le_cases([]) ->
    ok;
check_le_cases([{W, V, Plain, Prefixed} | T]) ->
    % construction, at bit offset 0 and after a 3-bit prefix
    Plain = make_le(id(V), id(W)),
    Prefixed = make_le_prefixed(id(V), id(W)),
    % extraction (the inverse mapping), from byte-layout literals
    <<X:W/little>> = id(Plain),
    V = X,
    <<_:3, Y:W/little>> = id(Prefixed),
    V = Y,
    check_le_cases(T).

make_le(V, W) -> <<V:W/little>>.
make_le_prefixed(V, W) -> <<5:3, V:W/little>>.

% Literal matches that consume trailing bits must measure the source capacity
% in bits, not whole bytes.
test_bs_match_string_trailing() ->
    ok = match_9(id(<<255, 1:1>>)),
    nomatch = match_9(id(<<255, 0:1>>)),
    nomatch = match_9(id(<<255>>)),
    ok = match_long(id(<<"hello!!!", 5:3>>)),
    nomatch = match_long(id(<<"hello!!!", 4:3>>)),
    ok.

match_9(<<255, 1:1>>) -> ok;
match_9(_) -> nomatch.

match_long(<<"hello!!!", 5:3>>) -> ok;
match_long(_) -> nomatch.

% Dynamic-size binary/bitstring segment extraction compiles to bs_get_binary2
% (OTP 26 through at least 29 emit it; fixed sizes go through bs_match).
% Sizes are bit-granular and the source offset may be unaligned.
test_dynamic_size_extraction() ->
    {<<1, 2>>, <<3>>} = dyn_binary(id(2), id(<<1, 2, 3>>)),
    nope = dyn_binary(id(4), id(<<1, 2, 3>>)),
    {<<5:3>>, <<1:1>>} = dyn_bits(id(3), id(<<5:3, 1:1>>)),
    {<<255, 1:1>>, <<5:3>>} = dyn_bits(id(9), id(<<255, 1:1, 5:3>>)),
    nope = dyn_bits(id(5), id(<<5:3, 1:1>>)),
    % dynamic size at an unaligned offset
    {<<2:2>>, <<1:1>>} = dyn_bits_after3(id(2), id(<<5:3, 2:2, 1:1>>)),
    % an all-remaining binary tail fails on a non-byte-aligned remainder
    nope = bin_tail_of(id(<<1:12>>)),
    % a negative dynamic size fails the match, it does not raise
    nope = dyn_binary(id(-1), id(<<1, 2, 3>>)),
    nope = dyn_bits(id(-1), id(<<5:3, 1:1>>)),
    % a bound segment with a unit other than 8 is only supported since
    % bit-granular extraction; a negative size must still be rejected before it
    % is scaled, as it is for the units the parent commit already covers
    nope = dyn_binary_unit64(id(-1), id(<<1>>)),
    nope = dyn_binary_unit64(id(-(1 bsl 58)), id(<<1>>)),
    nope = dyn_binary_unit64(id(-(1 bsl 58) - 1), id(<<1>>)),
    % a positive size whose scaling by the unit wraps must also fail the match:
    % (1 bsl 58) * 64 wraps to 0 on a 64-bit target, so without an overflow-aware
    % scaler the JIT would match an empty slice while BEAM/interpreter fail.
    nope = dyn_binary_unit64(id(1 bsl 58), id(<<1>>)),
    nope = dyn_binary_unit64(id(1 bsl 62), id(<<1>>)),
    nope = u3(id(-(1 bsl 62)), id(<<5:6, 1:3>>)),
    ok.

% A non-power-of-two unit on a variable-size binary segment (BEAM emits
% bs_get_binary2 with that unit); the JIT must handle what the interpreter does.
test_non_pow2_unit() ->
    {<<5:6>>, <<1:3>>} = u3(id(2), id(<<5:6, 1:3>>)),
    nope = u3(id(4), id(<<5:6, 1:3>>)),
    % `all` with a non-power-of-two unit: the remainder must be tested with a
    % real remainder, not a mask, and the JIT must agree with the interpreter
    <<5:6, 1:3>> = u3_all(id(<<5:6, 1:3>>)),
    <<1:1, 2:2>> = u3_all(id(<<1:1, 2:2>>)),
    <<>> = u3_all(id(<<>>)),
    nope = u3_all(id(<<5:6, 1:2>>)),
    nope = u3_all(id(<<1>>)),
    % same, at a non-byte-aligned starting offset
    <<3:3, 1:3>> = u3_all_after5(id(<<9:5, 3:3, 1:3>>)),
    nope = u3_all_after5(id(<<9:5, 3:2>>)),
    ok.

u3(N, B) ->
    case B of
        <<X:N/binary-unit:3, R/bitstring>> -> {X, R};
        _ -> nope
    end.

u3_all(B) ->
    case B of
        <<X/binary-unit:3>> -> X;
        _ -> nope
    end.

u3_all_after5(B) ->
    case B of
        <<_:5, X/binary-unit:3>> -> X;
        _ -> nope
    end.

% A signed integer of the full 64-bit width at a non-byte-aligned offset must
% sign-extend correctly (guards a shift by the whole type width in the extractor).
test_signed_int_unaligned() ->
    AllOnes = id(<<16#0F, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#F0>>),
    -1 = sig64_at4(AllOnes),
    18446744073709551615 = uns64_at4(AllOnes),
    -1000000000000 = sig64_at4(id(<<0:4, -1000000000000:64/signed, 0:4>>)),
    42 = sig64_at4(id(<<0:4, 42:64/signed, 0:4>>)),
    ok.

% A zero-width field has no sign bit to extend. Only a runtime size reaches the
% extractor: the compiler folds a literal 0 away.
test_signed_int_zero_width() ->
    0 = sig_at(id(<<>>), id(0)),
    0 = sig_at(id(<<16#FF>>), id(0)),
    0 = sig_at_little(id(<<16#FF>>), id(0)),
    % a sign bit that is present still extends, at the same call site
    -1 = sig_at(id(<<16#FF>>), id(8)),
    ok.

sig_at(B, N) ->
    <<X:N/signed-integer, _/bitstring>> = B,
    X.

sig_at_little(B, N) ->
    <<X:N/little-signed-integer, _/bitstring>> = B,
    X.

% A >64-bit integer field at a non-byte-aligned offset is not supported yet
% (BEAM matches it); AtomVM raises unsupported, as the construction side does.
test_big_int_unaligned_unsupported() ->
    ok = atom_unsupported(fun() -> big72_at1(id(<<0:1, 42:72, 0:7>>)) end),
    % a runtime size takes the legacy bs_get_integer2 path, which must raise
    % unsupported too rather than failing the match
    ok = atom_unsupported(fun() -> bigdyn_at1(id(<<0:1, 42:72, 0:7>>), id(72)) end),
    % byte-aligned offset and size are supported at any width up to 256 bits.
    % The value must not fit in 64 bits: a >64-bit field yields an unnormalized
    % bignum, so a small value would not compare equal to a small integer.
    (1 bsl 70) = big72_at0(id(<<(1 bsl 70):72, 0:8>>)),
    ok.

big72_at1(B) ->
    <<_:1, X:72, _:7>> = B,
    X.

big72_at0(B) ->
    <<X:72, _:8>> = B,
    X.

bigdyn_at1(B, N) ->
    <<_:1, X:N, _:7>> = B,
    X.

sig64_at4(B) ->
    <<_:4, X:64/signed, _:4>> = B,
    X.

uns64_at4(B) ->
    <<_:4, X:64/unsigned, _:4>> = B,
    X.

dyn_binary(N, B) ->
    case B of
        <<A:N/binary, Rest/binary>> -> {A, Rest};
        _ -> nope
    end.

dyn_bits(N, B) ->
    case B of
        <<X:N/bitstring, R/bits>> -> {X, R};
        _ -> nope
    end.

dyn_binary_unit64(N, B) ->
    case B of
        <<X:N/binary-unit:64, R/bits>> -> {X, R};
        _ -> nope
    end.

dyn_bits_after3(N, B) ->
    case B of
        <<_:3, X:N/bits, R/bits>> -> {X, R};
        _ -> nope
    end.

bin_tail_of(B) ->
    case B of
        <<X/binary>> -> X;
        _ -> nope
    end.

sort_bitstrings(L) -> sort_bitstrings(L, []).

sort_bitstrings([], Sorted) -> Sorted;
sort_bitstrings([H | T], Sorted) -> sort_bitstrings(T, insert_bitstring(Sorted, H)).

insert_bitstring([], B) -> [B];
insert_bitstring([H | T], B) when B < H -> [B, H | T];
insert_bitstring([H | T], B) -> [H | insert_bitstring(T, B)].

test_create_with_int_little_endian() ->
    <<2, 1>> = create_int_binary_little_endian(16#0102, 16),
    <<254, 255>> = create_int_binary_little_endian(16#FFFE, 16),
    <<4, 3, 2, 1>> = create_int_binary_little_endian(16#01020304, 32),
    <<252, 253, 254, 255>> = create_int_binary_little_endian(16#FFFEFDFC, 32),
    <<0>> = create_int_binary_little_endian(1024, 8),

    <<0, 2, 1>> = create_int_binary_little_endian(8, 16#0102, 16),
    <<0, 254, 255>> = create_int_binary_little_endian(8, 16#FFFE, 16),
    <<0, 4, 3, 2, 1>> = create_int_binary_little_endian(8, 16#01020304, 32),
    <<0, 252, 253, 254, 255>> = create_int_binary_little_endian(8, 16#FFFEFDFC, 32),

    <<0, 0, 2, 1>> = create_int_binary_little_endian(16, 16#0102, 16),
    <<0, 0, 254, 255>> = create_int_binary_little_endian(16, 16#FFFE, 16),
    <<0, 0, 4, 3, 2, 1>> = create_int_binary_little_endian(16, 16#01020304, 32),
    <<0, 0, 252, 253, 254, 255>> = create_int_binary_little_endian(16, 16#FFFEFDFC, 32),

    <<0, 0, 0, 2, 1>> = create_int_binary_little_endian(24, 16#0102, 16),
    <<0, 0, 0, 254, 255>> = create_int_binary_little_endian(24, 16#FFFE, 16),
    <<0, 0, 0, 4, 3, 2, 1>> = create_int_binary_little_endian(24, 16#01020304, 32),
    <<0, 0, 0, 252, 253, 254, 255>> = create_int_binary_little_endian(24, 16#FFFEFDFC, 32),
    ok.

test_create_with_int_signed() ->
    ok = expect_equals(<<0, 0, 255, 255>>, create_int_binary_signed(16#FFFF, 32)),
    ok.

test_create_with_invalid_binary_value() ->
    expect_error(fun() -> create_binary_binary(foo, id(32)) end, badarg).

test_create_with_invalid_binary_size() ->
    expect_error(fun() -> create_binary_binary(<<"foo">>, id(bar)) end, badarg).

test_create_with_binary_size_out_of_range() ->
    expect_error(fun() -> create_binary_binary(<<"foo">>, id(4)) end, badarg).

test_create_with_unsupported_binary_unit() ->
    % A binary segment whose size in bits is not a multiple of 8 takes a
    % bit-granular prefix of the source (9 bits of <<"foo">> here).
    <<102, 0:1>> = create_binary_binary_unit_3(<<"foo">>, id(3)),
    ok.

% Things are very broken here, we get {badmatch, <<16#FFFFFFFF:32>>} but
% this term isn't equal to {badmatch, <<16#FFFFFFFF:32>>}
test_get_with_unsupported_int_unit() ->
    atom_unsupported(
        fun() ->
            R = id(16#FF),
            R = get_integer_big_unsigned_unit_3(id(<<16#FFFFFFFF:32>>), id(8))
        end,
        fun(_) -> true end
    ).

test_get_with_int_little_endian() ->
    Bin1 = id(<<255, 254, 253, 252, 251, 250, 249, 248, 247, 246, 245, 244>>),
    Bin2 = id(<<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>),
    16#FEFF = get_integer_little_unsigned(id(Bin1), 16),
    16#0100 = get_integer_little_unsigned(id(Bin2), 16),
    16#FCFDFEFF = get_integer_little_unsigned(id(Bin1), 32),
    16#03020100 = get_integer_little_unsigned(id(Bin2), 32),
    %   16#F8F9FAFBFCFDFEFF = get_integer_little_unsigned(id(Bin1), 64),
    %   Even this fails as well until we have proper bigint support as we can't
    %   represent 16#F8F9FAFBFCFDFEFF unsigned
    %   X = get_integer_little_unsigned(id(Bin1), 64),
    %   16#F8F9FAFB = X bsr 32,
    %   16#FCFDFEFF = X band 16#FFFFFFFF,
    16#0706050403020100 = get_integer_little_unsigned(id(Bin2), 64),

    16#FDFE = get_integer_little_unsigned(8, id(Bin1), 16),
    16#0201 = get_integer_little_unsigned(8, id(Bin2), 16),
    16#FBFCFDFE = get_integer_little_unsigned(8, id(Bin1), 32),
    16#04030201 = get_integer_little_unsigned(8, id(Bin2), 32),
    %   16#F7F8F9FAFBFCFDFE = get_integer_little_unsigned(8, id(Bin1), 64),
    16#0807060504030201 = get_integer_little_unsigned(8, id(Bin2), 64),

    16#FCFD = get_integer_little_unsigned(16, id(Bin1), 16),
    16#0302 = get_integer_little_unsigned(16, id(Bin2), 16),
    16#FAFBFCFD = get_integer_little_unsigned(16, id(Bin1), 32),
    16#05040302 = get_integer_little_unsigned(16, id(Bin2), 32),
    %   16#F6F7F8F9FAFBFCFD = get_integer_little_unsigned(16, id(Bin1), 64),
    16#0908070605040302 = get_integer_little_unsigned(16, id(Bin2), 64),

    16#FBFC = get_integer_little_unsigned(24, id(Bin1), 16),
    16#0403 = get_integer_little_unsigned(24, id(Bin2), 16),
    16#F9FAFBFC = get_integer_little_unsigned(24, id(Bin1), 32),
    16#06050403 = get_integer_little_unsigned(24, id(Bin2), 32),
    %   16#F5F6F7F8F9FAFBFC = get_integer_little_unsigned(24, id(Bin1), 64),
    16#0A09080706050403 = get_integer_little_unsigned(24, id(Bin2), 64),

    ok.

test_get_with_int_signed() ->
    Bin1 = id(<<255, 254, 253, 252, 251, 250, 249, 248, 247, 246, 245, 244>>),
    Bin2 = id(<<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>),
    Bin3 = id(<<128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139>>),
    -16#0002 = get_integer_big_signed(id(Bin1), 16),
    16#0001 = get_integer_big_signed(id(Bin2), 16),
    -16#00010204 = get_integer_big_signed(id(Bin1), 32),
    16#00010203 = get_integer_big_signed(id(Bin2), 32),
    -16#01020304050608 = get_integer_big_signed(id(Bin1), 64),
    16#01020304050607 = get_integer_big_signed(id(Bin2), 64),
    -16#7F7E7D7C7B7A7979 = get_integer_big_signed(id(Bin3), 64),

    -16#0103 = get_integer_big_signed(8, id(Bin1), 16),
    16#0102 = get_integer_big_signed(8, id(Bin2), 16),
    -16#01020305 = get_integer_big_signed(8, id(Bin1), 32),
    16#01020304 = get_integer_big_signed(8, id(Bin2), 32),
    -16#0102030405060709 = get_integer_big_signed(8, id(Bin1), 64),
    16#0102030405060708 = get_integer_big_signed(8, id(Bin2), 64),
    -16#7E7D7C7B7A797878 = get_integer_big_signed(8, id(Bin3), 64),

    -16#0204 = get_integer_big_signed(16, id(Bin1), 16),
    16#0203 = get_integer_big_signed(16, id(Bin2), 16),
    -16#02030406 = get_integer_big_signed(16, id(Bin1), 32),
    16#02030405 = get_integer_big_signed(16, id(Bin2), 32),
    -16#020304050607080A = get_integer_big_signed(16, id(Bin1), 64),
    16#0203040506070809 = get_integer_big_signed(16, id(Bin2), 64),
    -16#7D7C7B7A79787777 = get_integer_big_signed(16, id(Bin3), 64),

    -16#0305 = get_integer_big_signed(24, id(Bin1), 16),
    16#0304 = get_integer_big_signed(24, id(Bin2), 16),
    -16#03040507 = get_integer_big_signed(24, id(Bin1), 32),
    16#03040506 = get_integer_big_signed(24, id(Bin2), 32),
    -16#030405060708090B = get_integer_big_signed(24, id(Bin1), 64),
    16#030405060708090A = get_integer_big_signed(24, id(Bin2), 64),
    -16#7C7B7A7978777676 = get_integer_big_signed(24, id(Bin3), 64),

    ok.

test_get_with_unaligned_binary() ->
    % A dynamic-size binary segment may start at an unaligned offset (the
    % extracted slice is copied in that case).
    {0, <<16>>} = get_int_then_binary(<<1, 2, 3, 4>>, id(4), id(1)),
    ok.

% A slice of 64 bytes or more is allocated as a refc binary instead of a heap
% binary. Extract such slices at a non-byte-aligned offset, where the bits have
% to be copied and cannot be shared with the source.
test_large_unaligned_slice() ->
    Payload = id(seq_binary(72, <<>>)),
    576 = bit_size(Payload),

    % fixed size, byte-aligned length at an unaligned offset
    Src1 = <<0:3, Payload/binary, 0:5>>,
    73 = byte_size(Src1),
    <<_:3, Slice1:72/binary, _:5>> = Src1,
    Payload = Slice1,
    true = is_binary(Slice1),

    % same, with a size only known at run time
    N = id(72),
    <<_:3, Slice2:N/binary, _:5>> = Src1,
    Payload = Slice2,

    % get_tail of a byte-aligned remainder at an unaligned offset
    Src2 = <<0:3, Payload/binary>>,
    579 = bit_size(Src2),
    <<_:3, Tail1/bits>> = Src2,
    true = is_binary(Tail1),
    Payload = Tail1,

    % get_tail of a remainder that is itself not byte-aligned: the copy is
    % wrapped in a sub-binary carrying the trailing bits
    Src3 = <<0:3, Payload/binary, 1:1>>,
    580 = bit_size(Src3),
    <<_:3, Tail2/bits>> = Src3,
    577 = bit_size(Tail2),
    false = is_binary(Tail2),
    <<Payload:72/binary, 1:1>> = Tail2,

    % a large slice that is not byte-aligned either: 2 remaining leading zero
    % bits followed by the first 569 bits of the payload
    <<_:1, Slice3:571/bits, _/bits>> = Src3,
    571 = bit_size(Slice3),
    false = is_binary(Slice3),
    <<PayloadPrefix:569/bits, _/bits>> = Payload,
    <<0:2, PayloadPrefix/bits>> = Slice3,
    ok.

seq_binary(0, Acc) -> Acc;
seq_binary(N, Acc) -> seq_binary(N - 1, <<(N - 1):8, Acc/binary>>).

% The accumulator of the destructive append path (private_append) may itself be
% a bitstring: its trailing bits must be preserved, and a result that is not
% byte-aligned must be returned as a bitstring.
test_private_append_bitstring() ->
    <<>> = accum_bits(id([])),
    <<5:3>> = accum_bits(id([5])),
    <<5:3, 2:3>> = accum_bits(id([5, 2])),
    6 = bit_size(accum_bits(id([5, 2]))),

    % after 8 three-bit segments the total is byte-aligned again, while the
    % accumulator entering the last append is not
    Eight = accum_bits(id([1, 2, 3, 4, 5, 6, 7, 0])),
    24 = bit_size(Eight),
    true = is_binary(Eight),
    <<1:3, 2:3, 3:3, 4:3, 5:3, 6:3, 7:3, 0:3>> = Eight,

    % long enough that the accumulator is a refc binary being reallocated
    Long = accum_bits(id(repeat(200, 5, []))),
    600 = bit_size(Long),
    true = is_binary(Long),
    <<5:3, 5:3, Rest/bits>> = Long,
    594 = bit_size(Rest),

    % appending a bitstring, rather than an integer segment, to an unaligned
    % accumulator
    <<1:3, 2:5, 3:4>> = accum_tail(<<1:3>>, <<2:5>>, <<3:4>>),
    12 = bit_size(accum_tail(<<1:3>>, <<2:5>>, <<3:4>>)),
    ok.

accum_bits(L) -> accum_bits(L, <<>>).

accum_bits([], Acc) -> Acc;
accum_bits([H | T], Acc) -> accum_bits(T, <<Acc/bitstring, H:3>>).

accum_tail(A, B, C) -> accum_tail2([A, B, C], <<>>).

accum_tail2([], Acc) -> Acc;
accum_tail2([H | T], Acc) -> accum_tail2(T, <<Acc/bitstring, H/bitstring>>).

repeat(0, _V, Acc) -> Acc;
repeat(N, V, Acc) -> repeat(N - 1, V, [V | Acc]).

create_int_binary_unit_3(Value, Size) ->
    <<Value:Size/integer-big-unit:3>>.

create_int_binary(Value, Size) ->
    <<Value:Size>>.

create_int_binary_little_endian(Value, Size) ->
    <<Value:Size/little>>.

create_int_binary_little_endian(Skip, Value, Size) ->
    <<0:Skip, Value:Size/little>>.

create_int_binary_signed(Value, Size) ->
    <<Value:Size/signed>>.

create_binary_binary(Value, Size) ->
    <<Value:Size/binary>>.

create_binary_binary_unit_3(Value, Size) ->
    <<Value:Size/binary-unit:3>>.

get_integer_big_unsigned(Bin, Size) ->
    <<Value:Size, _Rest/binary>> = Bin,
    Value.

get_integer_big_unsigned_unit_3(Bin, Size) ->
    <<Value:Size, _Rest/binary-unit:3>> = Bin,
    Value.

get_integer_little_unsigned(Bin, Size) ->
    <<Value:Size/little, _Rest/binary>> = Bin,
    Value.

get_integer_little_unsigned(Skip, Bin, Size) ->
    <<_:Skip, Value:Size/little, _Rest/binary>> = Bin,
    Value.

get_integer_big_signed(Bin, Size) ->
    <<Value:Size/signed, _Rest/binary>> = Bin,
    Value.

get_integer_big_signed(Skip, Bin, Size) ->
    <<_:Skip, Value:Size/signed, _Rest/binary>> = Bin,
    Value.

get_int_then_binary(Bin, IntSize, BinSize) ->
    <<IntValue:IntSize/integer, BinValue:BinSize/binary, _Rest/bitstring>> = Bin,
    {IntValue, BinValue}.

expect_equals(A, A) ->
    ok;
expect_equals(A, B) ->
    throw({not_equal, A, B}).

expect_error(F, Reason) when is_atom(Reason) orelse is_tuple(Reason) ->
    expect_error(F, fun(Tag, Value) -> Tag =:= error andalso Value =:= Reason end);
expect_error(F, ErrorValidator) when is_function(ErrorValidator) ->
    ok =
        try
            F(),
            unexpected
        catch
            T:V ->
                case ErrorValidator(T, V) of
                    false ->
                        erlang:display({T, V}),
                        {got, {T, V}, validator_failed};
                    true ->
                        ok
                end
        end.

atom_unsupported(F) ->
    atom_unsupported(F, fun(Reason) -> Reason =:= unsupported end).

atom_unsupported(F, ExpectedErrorValidator) ->
    R =
        try
            F(),
            ok
        catch
            error:Reason -> Reason
        end,
    case erlang:system_info(machine) of
        "BEAM" ->
            R = ok;
        _ ->
            true = ExpectedErrorValidator(R),
            ok
    end.

test_match_first_integer(<<16#FF:8, Rest/binary>>) ->
    Rest;
test_match_first_integer(<<16#ABCD:16, Rest/binary>>) ->
    Rest;
test_match_first_integer(_) ->
    nope.

test_bs_append(Bin1, Bin2) ->
    <<Bin1/binary, Bin2/binary>>.

test_bs_private_append(Bin) ->
    <<<<Byte:8>> || <<Byte:8>> <= Bin>>.

% This encodes with private_append and ensures we do this with buffers allocated on the heap
test_bs_private_append2(<<C1:8, C2:8, C3:8, C4:8, Rest/binary>>, Acc) when byte_size(Acc) < 16 ->
    test_bs_private_append2(
        Rest,
        <<Acc/binary, C1:4, C2:4, C3:4, C4:4, C1:4, C2:4, C3:4, C4:4, C1:4, C2:4, C3:4, C4:4, C1:4,
            C2:4, C3:4, C4:4, C1:4, C2:4, C3:4, C4:4>>
    );
test_bs_private_append2(<<C1:8, C2:8, Rest/binary>>, Acc) ->
    test_bs_private_append2(Rest, <<Acc/binary, C1:4, C2:4>>);
test_bs_private_append2(<<>>, Acc) ->
    Acc.

test_match_clause(
    <<$n:8, FixedBinaryData:4/binary, Rest/binary>>
) ->
    {$n, FixedBinaryData, Rest};
test_match_clause(
    <<$c:8, ChA:32, DiA:16/binary>>
) ->
    {$c, ChA, DiA};
test_match_clause(_) ->
    nope.

test_match_recursive(<<"">> = Empty, Accum) ->
    [Empty | Accum];
test_match_recursive(<<122:8, Rest/binary>>, Accum) ->
    test_match_recursive(Rest, [122 | Accum]);
test_match_recursive(<<119:32, Rest/binary>>, Accum) ->
    test_match_recursive(Rest, [119 | Accum]);
test_match_recursive(_SoFar, _Accum) ->
    nope.

test_match_force_gc(<<ReallyBigBin:1024/binary, Rest/binary>>) ->
    {ReallyBigBin, Rest}.

make_binary(Size) ->
    make_binary(Size, <<"">>).

make_binary(0, Accum) ->
    Accum;
make_binary(Size, Accum) ->
    Byte = Size rem 256,
    make_binary(Size - 1, <<Accum/binary, Byte:8>>).

make_binary_copy(0, _Byte, Acc) ->
    Acc;
make_binary_copy(Count, Byte, Acc) ->
    make_binary_copy(Count - 1, Byte, <<Byte, Acc/binary>>).

test_put_match_string(Prefix, Suffix) ->
    Bin = <<$f:8, $o:8, $o:8, Suffix/binary>>,
    <<Prefix:3/binary, $b:8, $a:8, $r:8>> = Bin.

test_skip_bits() ->
    <<"oobar">> = skip_bits(8, <<"foobar">>),
    <<"obar">> = skip_bits(16, <<"foobar">>),
    <<"">> = skip_bits(48, <<"foobar">>),
    ok = expect_error(fun() -> skip_bits(128, <<"foobar">>) end, {badmatch, <<"foobar">>}),
    ok = expect_error(fun() -> skip_bits(1, <<"foobar">>) end, fun skip_bits_unsupported/2),
    ok.

skip_bits_unsupported(Tag, Value) ->
    case erlang:system_info(machine) of
        "BEAM" -> Tag =:= error andalso Value =:= {badmatch, <<"foobar">>};
        % cannot match here, things are broken (and unsupported)
        _ -> true
    end.

skip_bits(Len, Bin) ->
    <<_First:Len, Rest/binary>> = Bin,
    Rest.

test_bs_match_string_unaligned() ->
    <<0:1, _:3, 42:7, _:5, 42>> = id(<<0:3, 42, 0:5, 42>>),
    <<0:1, _:3, 42:12, _:8, 42>> = id(<<0, 42, 0, 42>>),
    ok = expect_error(
        fun() -> <<0:1, _:4, 42:12, 0:7>> = id(<<0:5, 42, 0:3>>) end, {badmatch, <<1, 80>>}
    ),
    ok.

test_match_case_type() ->
    foo = match_case_type([foo, bar]),
    $a = match_case_type(<<"abc">>),
    something_else_entirely = match_case_type(blahblah),
    ok.

match_case_type(Term) ->
    case Term of
        [H | _T] ->
            H;
        <<H:8, _/binary>> ->
            H;
        _ ->
            something_else_entirely
    end.

-define(TEST_BINARY_DATA,
    <<241, 131, 104, 2, 100, 0, 4, 99, 97, 108, 108, 104, 2, 104, 3, 100, 0, 3, 106, 111, 101, 100,
        0, 6, 114, 111, 98, 101, 114, 116, 97, 0, 104, 2, 100, 0, 5, 104, 101, 108, 108, 111, 97,
        1>>
).

%% erlfmt-ignore
-define(TEST_LIST_DATA,
    [241, 131, 104, 2, 100, 0, 4, 99, 97, 108, 108, 104, 2, 104, 3, 100, 0, 3, 106, 111, 101,100,
        0, 6, 114, 111, 98, 101, 114, 116, 97, 0, 104, 2, 100, 0, 5, 104, 101, 108, 108, 111, 97,
        1]
).

test_iterate_binary() ->
    ?TEST_LIST_DATA = traverse(id(?TEST_BINARY_DATA), []),
    ok.

traverse(<<"">>, Accum) -> Accum;
traverse(<<H:8, T/binary>>, Accum) -> traverse(T, Accum ++ [H]).

test_large() ->
    X = <<42:1024>>,
    true = id(X) =:= <<42:1024>>,
    ok.

% The sequence 1:1,11:4,3:3 is converted to a string of 1 byte
% OTP 26 uses OP_BS_CREATE_BIN with STRING
test_copy_bits_string() ->
    A = id(42),
    X1 = id(0),
    X2 = id(0),
    Y1 = <<A:16/little, X1:7, 1:1, 11:4, 3:3, X2:1>>,
    <<42, 0, 1, 182>> = Y1,
    ok.

% With OTP27, this generates the following code:
%    {test,bs_start_match3,{f,197},1,[{x,0}],{x,0}}.
%    {bs_match,{f,197},
%              {x,0},
%              {commands,[{ensure_at_least,16,1},
%                         {integer,1,{literal,[]},16,1,{x,1}}]}}.
%    {bs_get_position,{x,0},{x,2},2}.
%    {select_val,{tr,{x,1},{t_integer,{0,65535}}},
%                {f,197},
%                {list,[{integer,24940},
%                       {f,196},
%                       {integer,28271},
%                       {f,195},
%                       {integer,28523},
%                       {f,193}]}}.
%  {label,193}.
%    {test,bs_match_string,{f,194},[{x,0},104,{string,<<"_simultaneous">>}]}.
%    {bs_match,{f,194},{x,0},{commands,[{ensure_exactly,0}]}}.
%    {move,{atom,ok},{x,0}}.
%    {jump,{f,198}}.
%
% We ensure here (using valgrind) that bs_match_string doesn't try to compare
% beyond heap where <<"okay">> is.
test_bs_match_string_select() ->
    R = <<"ok">>,
    L = <<"ay">>,
    Z =
        case join(R, L) of
            <<"ok">> -> nok_ok;
            <<"ok_simultaneous">> -> nok_simultaneous;
            <<"alive">> -> nok_alive;
            _Other -> ok
        end,
    id(Z).

% OTP 26+ uses bs_match for this pattern
test_bs_skip_bits2_little() ->
    ok = check_x86_64_jt(id(<<16#e9, 0:32>>)).

test_bs_variable_size_bitstring() ->
    B1 = id(<<1, 2>>),
    B2 = id(<<3, 4>>),
    S1 = id(16),
    S2 = id(8),
    <<1, 2, 3, 4>> = <<B1:S1/bitstring, B2/binary>>,
    <<1, 2, 3>> = <<B1:S1/bitstring, B2:S2/bitstring>>,
    S3 = id(all),
    % AtomVM emu flavor actually accepts a dynamic all because a literal term
    % is evaluated like a variable one. BEAM and jit flavor don't.
    SupportsVariableAll =
        case erlang:system_info(machine) of
            "BEAM" ->
                no;
            "ATOM" ->
                case erlang:system_info(emu_flavor) of
                    jit -> no;
                    emu -> ok
                end
        end,
    ok =
        try
            <<1, 2, 3, 4>> = <<B1:S1/bitstring, B2:S3/bitstring>>,
            SupportsVariableAll
        catch
            error:badarg ->
                ok
        end,
    ok.

test_float() ->
    FloatSize =
        case erlang:system_info(machine) of
            "BEAM" -> 8;
            "ATOM" -> erlang:system_info(avm_floatsize)
        end,

    Pi = id(3.14),
    case FloatSize of
        8 ->
            <<64, 9, 30, 184, 81, 235, 133, 31, 3, 14>> = <<Pi/float, 3, 14>>,
            <<64, 9, 30, 184, 81, 235, 133, 31, 3, 14>> = <<Pi/float-big, 3, 14>>,
            <<31, 133, 235, 81, 184, 30, 9, 64, 3, 14>> = <<Pi/float-little, 3, 14>>,
            <<_:64, 3, 14>> = <<Pi/float-native, 3, 14>>,
            <<64, 72, 245, 195, 3, 14>> = <<Pi:32/float, 3, 14>>,
            <<195, 245, 72, 64, 3, 14>> = <<Pi:32/float-little, 3, 14>>,

            <<Pi/float, 3, 14>> = id(<<64, 9, 30, 184, 81, 235, 133, 31, 3, 14>>),
            <<Pi/float-little, 3, 14>> = id(<<31, 133, 235, 81, 184, 30, 9, 64, 3, 14>>),
            <<PiS:32/float, 3, 14>> = id(<<64, 72, 245, 195, 3, 14>>),
            <<PiS:32/float-little, 3, 14>> = id(<<195, 245, 72, 64, 3, 14>>),
            true = abs(PiS - Pi) < 0.0001;
        4 ->
            <<64, 9, 30, 184, 96, 0, 0, 0, 3, 14>> = <<Pi/float, 3, 14>>,
            <<64, 9, 30, 184, 96, 0, 0, 0, 3, 14>> = <<Pi/float-big, 3, 14>>,
            <<0, 0, 0, 96, 184, 30, 9, 64, 3, 14>> = <<Pi/float-little, 3, 14>>,
            <<_:64, 3, 14>> = <<Pi/float-native, 3, 14>>,
            <<64, 72, 245, 195, 3, 14>> = <<Pi:32/float, 3, 14>>,
            <<195, 245, 72, 64, 3, 14>> = <<Pi:32/float-little, 3, 14>>,

            <<Pi/float, 3, 14>> = id(<<64, 9, 30, 184, 96, 0, 0, 0, 3, 14>>),
            <<Pi/float-little, 3, 14>> = id(<<0, 0, 0, 96, 184, 30, 9, 64, 3, 14>>),
            <<Pi:32/float, 3, 14>> = id(<<64, 72, 245, 195, 3, 14>>),
            <<Pi:32/float-little, 3, 14>> = id(<<195, 245, 72, 64, 3, 14>>)
    end,

    % Test integer to float conversion
    Int2 = id(2),
    IntNeg2 = id(-2),
    Int32 = id(32),
    <<64, 0, 0, 0, 0, 0, 0, 0>> = <<Int2/float>>,
    <<192, 0, 0, 0, 0, 0, 0, 0>> = <<IntNeg2/float>>,
    <<66, 0, 0, 0>> = <<Int32:32/float>>,

    Pi16 = id(3.14),
    <<66, 72>> = <<Pi16:16/float>>,
    <<66, 72>> = <<Pi16:16/float-big>>,
    <<72, 66>> = <<Pi16:16/float-little>>,
    <<Pi16B:16/float, 3, 14>> = <<66, 72, 3, 14>>,
    <<Pi16B:16/float-little, 3, 14>> = <<72, 66, 3, 14>>,
    true = abs(Pi16B - Pi16) < 0.001,

    ok = test_integer_outside_float_limits(),
    ok = test_create_with_invalid_float_value(),
    ok = test_create_with_invalid_float_size(),
    ok.

test_create_with_invalid_float_value() ->
    ok = expect_error(fun() -> create_float_binary(foo, id(64)) end, badarg),
    ok = expect_error(fun() -> create_float_binary([1, 2, 3], id(32)) end, badarg),
    ok = expect_error(fun() -> create_float_binary(<<"binary">>, id(64)) end, badarg),
    ok.

test_create_with_invalid_float_size() ->
    % These sizes are invalid in both BEAM and AtomVM
    ok = expect_error(fun() -> create_float_binary(3.14, id(8)) end, badarg),
    ok = expect_error(fun() -> create_float_binary(3.14, id(128)) end, badarg),
    ok = expect_error(fun() -> create_float_binary(3.14, id(foo)) end, badarg),
    ok.

test_integer_outside_float_limits() ->
    V = id(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),

    FloatSize =
        case erlang:system_info(machine) of
            "BEAM" -> 8;
            "ATOM" -> erlang:system_info(avm_floatsize)
        end,

    TestFun = fun() -> create_float_binary(V, id(64)) end,

    case FloatSize of
        4 ->
            expect_error(TestFun, badarg);
        8 ->
            <<79, 240, 0, 0, 0, 0, 0, 0>> = TestFun(),
            % Following tests cannot work with 32-bit floats, since we are not able to build
            % an intermediate 32-bit float term.

            % Result is inf, so it cannot be deserialized back
            <<127, 128, 0, 0>> = create_float_binary(V, id(32)),
            <<255, 128, 0, 0>> = create_float_binary(-V, id(32)),

            <<124, 0>> = create_float_binary(V, id(16)),
            <<252, 0>> = create_float_binary(-V, id(16)),
            ok
    end,
    ok.

create_float_binary(Value, Size) ->
    <<Value:Size/float>>.

check_x86_64_jt(<<>>) -> ok;
check_x86_64_jt(<<16#e9, _Offset:32/little, Tail/binary>>) -> check_x86_64_jt(Tail);
check_x86_64_jt(Bin) -> {unexpected, Bin}.

test_negative_dynamic_size() ->
    B = id(<<1>>),
    nope = skip_unit64(id(-1), B),
    nope = skip_unit64(id(-(1 bsl 58)), B),
    nope = skip_unit64(id(-(1 bsl 58) - 1), B),
    nope = skip_unit8(id(-1), B),
    nope = skip_unit8(id(-(1 bsl 61)), B),
    nope = int_unit64(id(-1), B),
    nope = int_unit64(id(-(1 bsl 58)), B),
    nope = float_unit64(id(-1), id(<<1, 2, 3, 4, 5, 6, 7, 8>>)),
    nope = bin_unit8(id(-1), B),
    nope = bin_unit8(id(-(1 bsl 61)), B),
    % the same segments still match when the size is valid
    <<>> = skip_unit8(id(1), B),
    {<<1>>, <<>>} = bin_unit8(id(1), B),
    ok.

test_oversized_dynamic_size() ->
    B = id(<<1>>),
    nope = skip_unit64(id(1 bsl 26), B),
    nope = skip_unit64(id(1 bsl 58), B),
    nope = skip_unit8(id(1 bsl 26), B),
    nope = int_unit64(id(1 bsl 26), B),
    nope = int_unit64(id(1 bsl 58), B),
    nope = float_unit64(id(1 bsl 26), id(<<1, 2, 3, 4, 5, 6, 7, 8>>)),
    nope = bin_unit8(id(1 bsl 26), B),
    nope = skip_unit64(id(1 bsl 61), B),
    nope = int_unit64(id(1 bsl 61), B),
    nope = bin_unit8(id(1 bsl 61), B),
    ok.

% Construction (bs_create_bin) with a size whose scaling by the segment unit
% wraps must fail rather than silently truncate: (1 bsl 58) * 64 wraps to 0 on a
% 64-bit target, so without an overflow check the source-capacity test and the
% allocation size would both be bypassed. BEAM raises system_limit; AtomVM may
% raise system_limit or badarg for the binary-source case, so accept either.
test_construction_size_overflow() ->
    ok = expect_construction_error(fun() -> make_bin_unit64(id(<<>>), id(1 bsl 58)) end),
    ok = expect_construction_error(fun() -> make_bin_unit64(id(<<1>>), id(1 bsl 58)) end),
    ok = expect_construction_error(fun() -> make_int_unit64(id(0), id(1 bsl 58)) end),
    ok = expect_construction_error(fun() -> make_int_unit64(id(0), id(1 bsl 62)) end),
    ok.

make_bin_unit64(B, N) -> <<B:N/binary-unit:64>>.
make_int_unit64(V, N) -> <<V:N/integer-unit:64>>.

expect_construction_error(Fun) ->
    try Fun() of
        R -> {unexpected, R}
    catch
        error:badarg -> ok;
        error:system_limit -> ok
    end.

% Integers narrower than the field but wider than 64 bits must sign-extend a
% negative value (fill the high bits with 1s), not zero-extend it, and this must
% hold for both endiannesses. Expected values match BEAM.
test_wide_integer_construction() ->
    <<255, 255, 255, 255, 255, 255, 255, 255, 1:1>> = wide_be(id(-1), id(65)),
    <<255, 255, 255, 255, 255, 255, 255, 255, 1:1>> = wide_le(id(-1), id(65)),
    <<255, 255, 255, 255, 255, 255, 255, 255, 127:7>> = wide_be(id(-1), id(71)),
    <<255, 255, 255, 255, 255, 255, 255, 253, 1:1>> = wide_be(id(-5), id(65)),
    <<251, 255, 255, 255, 255, 255, 255, 255, 127:7>> = wide_le(id(-5), id(71)),
    % positive values still zero-extend
    <<0, 0, 0, 0, 0, 0, 0, 2, 1:1>> = wide_be(id(5), id(65)),
    <<0, 0, 0, 0, 0, 0, 0, 0, 5:7>> = wide_be(id(5), id(71)),
    <<5, 0, 0, 0, 0, 0, 0, 0, 0:7>> = wide_le(id(5), id(71)),
    ok.

wide_be(V, N) -> <<V:N>>.
wide_le(V, N) -> <<V:N/little>>.

skip_unit64(N, B) ->
    case B of
        <<_:N/binary-unit:64, R/binary>> -> R;
        _ -> nope
    end.

skip_unit8(N, B) ->
    case B of
        <<_:N/binary-unit:8, R/binary>> -> R;
        _ -> nope
    end.

bin_unit8(N, B) ->
    case B of
        <<X:N/binary-unit:8, R/binary>> -> {X, R};
        _ -> nope
    end.

int_unit64(N, B) ->
    case B of
        <<X:N/integer-unit:64, _/binary>> -> X;
        _ -> nope
    end.

float_unit64(N, B) ->
    case B of
        <<X:N/float-unit:64, _/binary>> -> X;
        _ -> nope
    end.

id(X) -> ?MODULE:ext_id(X).

ext_id(X) -> X.

join(X, Y) ->
    <<X/binary, Y/binary>>.
