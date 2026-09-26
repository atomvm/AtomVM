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

%% @doc Test legacy binary construction opcodes.
%% OTP 26-27 with no_bs_create_bin emit among others bs_add (111),
%% bs_init_bits (137), bs_put_integer (89), bs_append (134),
%% bs_put_binary (90), bs_put_float (91), bs_put_string (92),
%% bs_private_append (135), bs_utf8_size (144), bs_put_utf8 (145),
%% bs_utf16_size (146), bs_put_utf16 (147), bs_put_utf32 (148).
%% OTP 26-27 default and OTP 28+ emit bs_create_bin (177) instead.
-module(test_no_bs_create_bin).

-export([
    start/0,
    make_bin/2,
    append_byte/2,
    append_int32/2,
    concat_bins/2,
    put_float64/1,
    put_string_and_int/1,
    put_utf8/1,
    put_utf16/1,
    put_utf32/1,
    build_from_list/1,
    bin_comprehension/1,
    ext_id/1
]).

-if(?OTP_RELEASE =< 27).
%% no_type_opt is required to work around an OTP compiler bug where
%% beam_clean:fix_bs_create_bin/1 doesn't unwrap {tr, Reg, Type}
%% typed register annotations, causing beam_validator to reject
%% bs_private_append instructions.
-compile([no_bs_create_bin, no_type_opt]).
-endif.

start() ->
    ok = test_make_bin(),
    ok = test_append_byte(),
    ok = test_append_int32(),
    ok = test_concat_bins(),
    ok = test_put_float(),
    ok = test_put_string(),
    ok = test_put_utf8(),
    ok = test_put_utf16(),
    ok = test_put_utf32(),
    ok = test_private_append(),
    ok = test_bin_comprehension(),
    ok = test_bitstring_source(),
    0.

%% bs_add + bs_init_bits + bs_put_integer with dynamic size.
test_make_bin() ->
    <<0, 0, 42>> = make_bin(3, 42),
    <<7>> = make_bin(1, 7),
    <<1, 0>> = make_bin(2, 256),
    ok.

%% bs_add + bs_append + bs_put_integer with fixed size.
test_append_byte() ->
    <<1, 2>> = append_byte(<<1>>, 2),
    <<10, 20, 30>> = append_byte(<<10, 20>>, 30),
    ok.

%% bs_add + bs_append + bs_put_integer (32-bit).
test_append_int32() ->
    <<1, 0, 0, 0, 42>> = append_int32(<<1>>, 42),
    ok.

%% bs_add + bs_append + bs_put_binary.
test_concat_bins() ->
    <<1, 2, 3, 4>> = concat_bins(<<1, 2>>, <<3, 4>>),
    <<>> = concat_bins(<<>>, <<>>),
    ok.

%% bs_add + bs_init_bits + bs_put_float.
test_put_float() ->
    <<64, 89, 16, 0, 0, 0, 0, 0>> = put_float64(100.0 + 1.0 / 4),
    ok.

%% bs_add + bs_init_bits + bs_put_string + bs_put_integer.
test_put_string() ->
    <<"hello", 42>> = put_string_and_int(42),
    <<"hello", 0>> = put_string_and_int(0),
    ok.

%% bs_utf8_size + bs_add + bs_init_bits + bs_put_utf8.
test_put_utf8() ->
    <<97>> = put_utf8($a),
    <<194, 169>> = put_utf8(16#A9),
    <<224, 174, 164>> = put_utf8(16#0BA4),
    <<240, 144, 144, 183>> = put_utf8(16#10437),
    ok.

%% bs_utf16_size + bs_add + bs_init_bits + bs_put_utf16.
test_put_utf16() ->
    <<0, 97>> = put_utf16($a),
    <<216, 1, 220, 55>> = put_utf16(16#10437),
    ok.

%% bs_add + bs_init_bits + bs_put_utf32.
test_put_utf32() ->
    <<0, 0, 0, 97>> = put_utf32($a),
    <<0, 1, 4, 55>> = put_utf32(16#10437),
    ok.

%% bs_append (accumulator pattern).
test_private_append() ->
    <<1, 2, 3>> = build_from_list([1, 2, 3]),
    <<>> = build_from_list([]),
    <<42>> = build_from_list([42]),
    ok.

%% bs_private_append (binary comprehension, compiler knows result is unique).
test_bin_comprehension() ->
    <<1, 2, 3>> = bin_comprehension([1, 2, 3]),
    <<>> = bin_comprehension([]),
    <<42>> = bin_comprehension([42]),
    ok.

%% bs_append / bs_private_append / bs_put_binary with a non-byte-aligned
%% source. These opcodes are bit-granular: a partial bitstring keeps its
%% trailing bits in the result and every segment written after it shifts by
%% that many bits, exactly as bs_create_bin does. A /binary segment (unit 8)
%% still rejects a partial source with badarg, as on BEAM.
test_bitstring_source() ->
    %% a byte-aligned source is unaffected
    <<1, 2, 3, 4>> = concat_bins(id(<<1, 2>>), id(<<3, 4>>)),
    <<1, 2, 3>> = build_from_list(id([1, 2, 3])),
    <<1, 2>> = copy_bitstring(id(<<1, 2>>)),
    <<>> = copy_bitstring(id(<<>>)),
    <<1, 2, 3>> = build_from_list_onto(id(<<>>), [1, 2, 3]),

    Bits1 = id(<<1:1>>),
    Bits3 = id(<<5:3>>),
    Bits9 = id(<<16#AB:8, 1:1>>),

    %% bs_append: a partial source is copied whole, alone and followed by a
    %% segment that shifts by the trailing bit count
    <<1:1>> = copy_bitstring(Bits1),
    <<5:3>> = copy_bitstring(Bits3),
    <<16#AB:8, 1:1>> = copy_bitstring(Bits9),
    <<1:1, 16#AB:8>> = append_byte_to_bitstring(Bits1, 16#AB),
    <<5:3, 16#AB:8>> = append_byte_to_bitstring(Bits3, 16#AB),
    <<16#AB:8, 1:1, 16#CD:8>> = append_byte_to_bitstring(Bits9, 16#CD),

    %% bs_put_binary with an explicit bit size, and with a partial source
    %% written at an already unaligned destination offset
    <<5:3>> = put_bitstring_sized(Bits3, 3),
    <<5:3, 1:1>> = append_bits_to_bitstring(Bits3, id(<<1:1>>)),
    <<16#AB:8, 1:1, 5:3>> = append_bits_to_bitstring(Bits9, Bits3),

    %% bs_private_append: the accumulator itself is a partial bitstring, so it
    %% cannot be grown in place
    <<1:1, 1, 2, 3>> = build_from_list_onto(Bits1, [1, 2, 3]),
    <<5:3>> = build_from_list_onto(Bits3, []),

    %% a /binary segment rejects a partial source rather than truncating it
    badarg = expect_error(fun() -> append_binary_to(Bits1, id(<<1, 2>>)) end),

    %% a big integer (> 64 bits) written by the legacy bs_put_integer opcode at a
    %% non-byte-aligned offset is not supported: the whole-byte bignum writer
    %% cannot place it, so AtomVM rejects it rather than misplacing the bits.
    %% BEAM builds it, so check only on AtomVM.
    case erlang:system_info(machine) of
        "ATOM" ->
            %% the interpreter raises unsupported, the JIT badarg; either way it
            %% must reject rather than misplace the bits
            case expect_error(fun() -> put_wide_int_after(Bits1, id(1 bsl 100)) end) of
                unsupported -> ok;
                badarg -> ok
            end;
        _ ->
            ok
    end,
    ok.

put_wide_int_after(Bits, V) ->
    <<Bits/bitstring, V:101>>.

copy_bitstring(Bits) ->
    <<Bits/bitstring>>.

append_binary_to(Bits, Bin) ->
    <<Bits/binary, Bin/binary>>.

expect_error(Fun) ->
    try
        Fun(),
        no_error
    catch
        error:Reason -> Reason
    end.

append_byte_to_bitstring(Bits, Byte) ->
    <<Bits/bitstring, Byte:8>>.

append_bits_to_bitstring(Bits, More) ->
    <<Bits/bitstring, More/bitstring>>.

put_bitstring_sized(Bits, Size) ->
    <<Bits:Size/bitstring>>.

build_from_list_onto(Acc, []) ->
    Acc;
build_from_list_onto(Acc, [H | T]) ->
    build_from_list_onto(<<Acc/bitstring, H:8>>, T).

id(X) ->
    ?MODULE:ext_id(X).

ext_id(X) ->
    X.

make_bin(Size, Val) ->
    <<Val:Size/unit:8>>.

append_byte(Bin, Val) ->
    <<Bin/binary, Val:8>>.

append_int32(Bin, Val) ->
    <<Bin/binary, Val:32/integer>>.

concat_bins(A, B) ->
    <<A/binary, B/binary>>.

put_float64(Val) ->
    <<Val:64/float>>.

put_string_and_int(Val) ->
    <<"hello", Val:8>>.

put_utf8(C) ->
    <<C/utf8>>.

put_utf16(C) ->
    <<C/utf16>>.

put_utf32(C) ->
    <<C/utf32>>.

build_from_list(List) ->
    build_from_list(List, <<>>).

build_from_list([], Acc) ->
    Acc;
build_from_list([H | T], Acc) ->
    build_from_list(T, <<Acc/binary, H:8>>).

bin_comprehension(List) ->
    <<<<X:8>> || X <- List>>.
