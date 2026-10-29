%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

%% @doc Bitstrings with the legacy bs_* instructions, when the compiler can
%% still emit them.
-module(test_no_bs_unaligned).

-export([start/0, ext_id/1]).

-if(?OTP_RELEASE =< 27).
%% no_type_opt works around a compiler bug that rejects bs_private_append.
-compile([no_bs_create_bin, no_type_opt]).
-endif.
-if(?OTP_RELEASE =< 28).
-compile([no_bs_match]).
-endif.

-include("test_bs_bigint_common.hrl").

start() ->
    ok = test_bs_add(),
    ok = test_alloc_lists(),
    ok = test_put_utf(),
    ok = test_put_float(),
    ok = test_put_integer(),
    ok = test_put_string_and_binary(),
    ok = test_accumulators(),
    ok = test_errors(),
    ok = test_get_utf(),
    ok = test_get_float_and_integer(),
    ok = test_get_binary(),
    ok = test_match_string(),
    ok = test_skip_and_tests(),
    ok = bigint_test(),
    0.

id(X) -> ?MODULE:ext_id(X).

ext_id(X) -> X.

expect_error(Fun) ->
    try
        Fun(),
        no_error
    catch
        error:Reason -> Reason
    end.

test_bs_add() ->
    <<128, 129, 0:1>> = add_prefix_bit(id(<<1, 2>>)),
    <<1, 1, 2>> = add_prefix_byte(id(<<1, 2>>)),
    <<16, 16, 34>> = add_prefix_nibbles(id(<<1, 2>>)),
    <<1, 1, 2, 3, 4>> = add_prefix_byte(id(<<1, 2, 3, 4>>)),
    <<>> = add_prefix_none(id(<<>>)),
    <<1, 2>> = add_prefix_none(id(<<1, 2>>)),
    <<1:1>> = add_prefix_bit(id(<<>>)),
    ok.

add_prefix_bit(B) -> <<1:1, B/binary>>.
add_prefix_byte(B) -> <<1:8, B/binary>>.
add_prefix_nibbles(B) -> <<1:4, B/binary, 2:4>>.
add_prefix_none(B) -> <<B/binary>>.

test_alloc_lists() ->
    F1 = append_then_fun(id(<<1, 2>>), id(3)),
    <<1, 2, 3>> = F1(),
    F2 = append_then_fun(id(<<1:1>>), id(3)),
    <<1:1, 3>> = F2(),
    {F3, F4} = init_then_funs(id(3), id(5), id(7)),
    <<225, 9:5>> = F3(),
    3 = F4(),
    ok.

append_then_fun(Bin, V) ->
    B = <<Bin/bitstring, V:8>>,
    fun() -> B end.

init_then_funs(P, V, N) ->
    Pre = (1 bsl P) - 1,
    B = <<Pre:P, V:N, 1:3>>,
    {fun() -> B end, fun() -> P end}.

test_put_utf() ->
    C = id($a),
    <<176, 1:1>> = put_utf8_1(C),
    <<22, 18>> = put_utf8_4(C),
    <<88, 1:2>> = put_utf8_n(C, id(2)),
    <<128, 48, 1:1>> = put_utf16_1(C),
    <<128, 0, 0, 48, 1:1>> = put_utf32_1(C),
    Euro = id(16#20AC),
    Smile = id(16#1F600),
    <<241, 65, 86, 0:1>> = put_utf8_1(Euro),
    <<248, 79, 204, 64, 0:1>> = put_utf8_1(Smile),
    <<236, 30, 239, 0, 0:1>> = put_utf16_1(Smile),
    <<158, 236, 0, 111, 0:1>> = put_utf16_little_1(Smile),
    <<128, 123, 0, 128, 0:1>> = put_utf32_little_1(Smile),
    ok.

put_utf8_1(C) -> <<1:1, C/utf8>>.
put_utf8_4(C) -> <<1:4, C/utf8, 2:4>>.
put_utf8_n(C, N) -> <<1:N, C/utf8>>.
put_utf16_1(C) -> <<1:1, C/utf16>>.
put_utf16_little_1(C) -> <<1:1, C/little-utf16>>.
put_utf32_1(C) -> <<1:1, C/utf32>>.
put_utf32_little_1(C) -> <<1:1, C/little-utf32>>.

test_put_float() ->
    F = id(1.0),
    <<159, 192, 0, 0, 0:1>> = put_float32_1(F),
    <<19, 248, 0, 0, 2>> = put_float32_4(F),
    <<158, 0, 0:1>> = put_float16_1(F),
    <<128, 0, 0, 0, 0, 0, 120, 31, 1:1>> = put_float64_little_1(F),
    ok.

put_float32_1(F) -> <<1:1, F:32/float>>.
put_float32_4(F) -> <<1:4, F:32/float, 2:4>>.
put_float16_1(F) -> <<1:1, F:16/float>>.
put_float64_little_1(F) -> <<1:1, F:64/float-little>>.

test_put_integer() ->
    <<151>> = put_int_1(id(5)),
    <<222, 10:5>> = put_int_little_1(id(16#ABC)),
    <<207, 27, 188, 220, 191, 165, 62, 10, 1:1>> = put_int_1_64(id(16#9E3779B97F4A7C15)),
    <<192, 0, 0, 0, 0, 0, 0, 0, 0>> = put_int_1_71(id(1 bsl 70)),
    <<255, 255, 255, 255, 255, 255, 255, 255, 3:2>> = put_int_1_65(id(-1)),
    <<13:4>> = put_int_dyn(id(5), id(3)),
    <<1, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 2>> = put_big_middle(id(1 bsl 70)),
    <<1, 255, 192, 0, 0, 0, 0, 0, 0, 0, 0, 2>> = put_big_middle(id(-(1 bsl 70))),
    <<1, 0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 2>> = put_big_middle_little(id(1 bsl 70)),
    <<1, 0, 2>> = put_big_truncated(id(1 bsl 70)),
    ok.

put_int_1(V) -> <<1:1, V:5, 3:2>>.
put_int_little_1(V) -> <<1:1, V:12/little>>.
put_int_1_64(V) -> <<1:1, V:64>>.
put_int_1_71(V) -> <<1:1, V:71>>.
put_int_1_65(V) -> <<1:1, V:65>>.
put_int_dyn(V, N) -> <<1:1, V:N>>.
put_big_middle(V) -> <<1:8, V:80, 2:8>>.
put_big_middle_little(V) -> <<1:8, V:80/little, 2:8>>.
put_big_truncated(V) -> <<1:8, V:8, 2:8>>.

test_put_string_and_binary() ->
    <<176, 177, 49, 135>> = put_string_1(id(7)),
    <<128, 129, 0:1>> = put_binary_sized(id(<<1, 2, 3>>), id(2)),
    <<128, 4:3>> = put_bits_sized(id(<<1, 2, 3>>), id(10)),
    <<5:3, 7:3>> = put_bits(id(<<5:3>>), id(<<7:3>>)),
    <<5, 7>> = put_binaries(id(<<5>>), id(<<7>>)),
    <<167>> = put_bits(id(<<5:3>>), id(<<7:5>>)),
    ok.

put_string_1(V) -> <<1:1, "abc", V:7>>.
put_binary_sized(B, N) -> <<1:1, B:N/binary>>.
put_bits_sized(B, N) -> <<1:1, B:N/bitstring>>.
put_bits(A, B) -> <<A/bitstring, B/bitstring>>.
put_binaries(A, B) -> <<A/binary, B/binary>>.

test_accumulators() ->
    <<181, 1:1>> = accumulate_bits(id([1, 0, 1, 1, 0, 1, 0, 1, 1])),
    <<1, 2, 3>> = accumulate_bytes(id([1, 2, 3])),
    <<5:3>> = comprehension_bits(id([1, 0, 1])),
    <<6:3>> = comprehension_bitstrings(id([<<1:1>>, <<2:2>>])),
    ok.

accumulate_bits(L) -> accumulate_bits(L, <<>>).
accumulate_bits([], A) -> A;
accumulate_bits([H | T], A) -> accumulate_bits(T, <<A/bitstring, H:1>>).

accumulate_bytes(L) -> accumulate_bytes(L, <<>>).
accumulate_bytes([], A) -> A;
accumulate_bytes([H | T], A) -> accumulate_bytes(T, <<A/binary, H:8>>).

comprehension_bits(L) -> <<<<X:1>> || X <- L>>.
comprehension_bitstrings(L) -> <<<<X/bitstring>> || X <- L>>.

test_errors() ->
    badarg = expect_error(fun() -> add_prefix_bit(id(<<1:1>>)) end),
    badarg = expect_error(fun() -> put_binaries(id(<<5:3>>), id(<<7:3>>)) end),
    badarg = expect_error(fun() -> put_bits_then_binary(id(<<5:3>>), id(<<7:3>>)) end),
    badarg = expect_error(fun() -> put_int_dyn(id(5), id(-3)) end),
    badarg = expect_error(fun() -> put_zeros(id(-1)) end),
    badarg = expect_error(fun() -> put_binary_sized(id(<<1, 2>>), id(3)) end),
    badarg = expect_error(fun() -> put_binary_sized(id(<<1, 2>>), id(-1)) end),
    badarg = expect_error(fun() -> put_utf8_1(id(16#D800)) end),
    badarg = expect_error(fun() -> put_utf8_1(id(-1)) end),
    badarg = expect_error(fun() -> put_utf8_1(id(16#110000)) end),
    badarg = expect_error(fun() -> put_utf16_1(id(16#D800)) end),
    badarg = expect_error(fun() -> put_utf32_1(id(16#110000)) end),
    badarg = expect_error(fun() -> put_float32_1(id(a)) end),
    badarg = expect_error(fun() -> put_float_size(id(1.0), id(24)) end),
    badarg = expect_error(fun() -> put_int_dyn(id(a), id(8)) end),
    ok.

put_bits_then_binary(A, B) -> <<A/bitstring, B/binary>>.
put_zeros(N) -> <<0:N>>.
put_float_size(F, S) -> <<F:S/float>>.

test_get_utf() ->
    {97, <<>>} = get_utf8(id(<<1:1, $a/utf8>>)),
    {97, <<>>} = get_utf16(id(<<1:1, $a/utf16>>)),
    {97, <<>>} = get_utf32(id(<<1:1, $a/utf32>>)),
    nomatch = get_utf8(id(<<1:1, 16#C3:8>>)),
    nomatch = get_utf8(id(<<1:1, 16#C3, 1:7>>)),
    nomatch = get_utf16(id(<<1:1, 16#C3:8>>)),
    nomatch = get_utf32(id(<<1:1, 16#C3, 1:7>>)),
    {16#20AC, <<7:5>>} = get_utf8(id(<<5:1, 16#20AC/utf8, 7:5>>)),
    ok.

get_utf8(<<_:1, X/utf8, R/bits>>) -> {X, R};
get_utf8(_) -> nomatch.
get_utf16(<<_:1, X/utf16, R/bits>>) -> {X, R};
get_utf16(_) -> nomatch.
get_utf32(<<_:1, X/utf32, R/bits>>) -> {X, R};
get_utf32(_) -> nomatch.

test_get_float_and_integer() ->
    {1.0, <<>>} = get_float32(id(<<1:1, 1.0:32/float>>)),
    {1, 5, 3} = get_ints(id(<<1:1, 5:7, 3:3>>)),
    ok.

get_float32(<<_:1, X:32/float, R/bits>>) -> {X, R};
get_float32(_) -> nomatch.
get_ints(<<A:1, B:7, C:3>>) -> {A, B, C};
get_ints(_) -> nomatch.

test_get_binary() ->
    {<<7, 8>>, <<5:3>>} = get_binary_2(id(<<1:1, 7, 8, 5:3>>)),
    <<7, 8>> = get_binary_all(id(<<1:1, 7, 8>>)),
    nomatch = get_binary_all(id(<<1:1, 7, 8, 5:3>>)),
    <<7, 0:3>> = get_bitstring_all(id(<<1:1, 7, 0:3>>)),
    <<8, 5:3>> = get_tail_after_9(id(<<1:1, 7, 8, 5:3>>)),
    {<<7, 0:1>>, <<17, 1:2>>} = get_bits_dyn(id(<<1:1, 7, 8, 5:3>>), id(9)),
    nomatch = get_bits_dyn(id(<<1:1, 7, 8, 5:3>>), id(-9)),
    nomatch = get_bits_dyn(id(<<1:1, 7, 8, 5:3>>), id(1 bsl 58)),
    {0, <<5:7>>} = get_int_64(id(<<1:1, 0:64, 5:7>>)),
    ok.

get_binary_2(<<_:1, X:2/binary, R/bits>>) -> {X, R};
get_binary_2(_) -> nomatch.
get_binary_all(<<_:1, X/binary>>) -> X;
get_binary_all(_) -> nomatch.
get_bitstring_all(<<_:1, X/bitstring>>) -> X;
get_bitstring_all(_) -> nomatch.
get_tail_after_9(<<_:1, _:8, R/bits>>) -> R.
get_bits_dyn(B, N) ->
    case B of
        <<_:1, X:N/bits, R/bits>> -> {X, R};
        _ -> nomatch
    end.
get_int_64(<<_:1, X:64, R/bits>>) -> {X, R};
get_int_64(_) -> nomatch.

test_match_string() ->
    <<>> = match_string(id(<<1:1, "abc">>)),
    nomatch = match_string(id(<<1:1, "abd">>)),
    nomatch = match_string(id(<<"abc">>)),
    <<3:3>> = match_string_start(id(<<"abc", 3:3>>)),
    nomatch = match_string_start(id(<<"ab", 3:3>>)),
    ok.

match_string(<<_:1, "abc", R/bits>>) -> R;
match_string(_) -> nomatch.
match_string_start(<<"abc", R/bits>>) -> R;
match_string_start(_) -> nomatch.

test_skip_and_tests() ->
    <<8, 5:3>> = skip_9(id(<<1:1, 7, 8, 5:3>>)),
    nomatch = unit_3(id(<<1:1, 7, 8, 5:3>>)),
    <<7, 8, 20:5>> = unit_3(id(<<1:1, 7, 8, 5:3, 0:2>>)),
    ok.

skip_9(<<_:1, _:8, R/bits>>) -> R;
skip_9(_) -> nomatch.
unit_3(<<_:1, R/binary-unit:3>>) -> R;
unit_3(_) -> nomatch.
