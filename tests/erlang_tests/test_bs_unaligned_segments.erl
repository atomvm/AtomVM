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

%% Segments other than plain small integers at a bit offset that is not a
%% multiple of 8. Values go through id/1 so the segments are built and matched
%% at runtime instead of being folded into literals by the compiler.
-module(test_bs_unaligned_segments).

-export([start/0, id/1]).

-spec start() -> 0.
start() ->
    ok = test_utf8(),
    ok = test_utf16(),
    ok = test_utf32(),
    ok = test_uint64_bignum(),
    ok = test_float(),
    0.

test_utf8() ->
    C = ?MODULE:id($a),
    <<176, 1:1>> = <<1:1, C/utf8>>,
    <<22, 18>> = <<1:4, C/utf8, 2:4>>,
    {97, <<>>} = (fun(<<_:1, X/utf8, R/bits>>) -> {X, R} end)(?MODULE:id(<<1:1, $a/utf8>>)),
    {1, 97, 2} = (fun(<<A:4, X/utf8, B:4>>) -> {A, X, B} end)(?MODULE:id(<<1:4, $a/utf8, 2:4>>)),
    %% Multi-byte encodings
    Euro = ?MODULE:id(16#20AC),
    Smile = ?MODULE:id(16#1F600),
    <<_:3, Euro/utf8, Smile/utf8, _:5>> = <<5:3, Euro/utf8, Smile/utf8, 7:5>>,
    {16#20AC, 16#1F600, <<7:5>>} =
        (fun(<<_:3, X/utf8, Y/utf8, R/bits>>) -> {X, Y, R} end)(
            ?MODULE:id(<<5:3, 16#20AC/utf8, 16#1F600/utf8, 7:5>>)
        ),
    %% Fewer than 8 bits after a valid lead byte, or a truncated sequence
    nomatch = match_utf8_after_one_bit(?MODULE:id(<<1:1, 16#C3, 1:7>>)),
    nomatch = match_utf8_after_one_bit(?MODULE:id(<<1:1, 16#E2, 16#82>>)),
    ok.

match_utf8_after_one_bit(Bin) ->
    case Bin of
        <<_:1, X/utf8, _/bits>> -> X;
        _ -> nomatch
    end.

test_utf16() ->
    C = ?MODULE:id($a),
    <<128, 48, 1:1>> = <<1:1, C/utf16>>,
    97 = (fun(<<_:1, X/utf16>>) -> X end)(?MODULE:id(<<1:1, $a/utf16>>)),
    97 = (fun(<<_:1, X/little-utf16>>) -> X end)(?MODULE:id(<<1:1, $a/little-utf16>>)),
    %% A surrogate pair
    Smile = ?MODULE:id(16#1F600),
    Bin = <<1:2, Smile/utf16, 1:6>>,
    16#1F600 = (fun(<<_:2, X/utf16, _:6>>) -> X end)(?MODULE:id(Bin)),
    Bin = <<1:2, 16#1F600/utf16, 1:6>>,
    LBin = <<1:2, Smile/little-utf16, 1:6>>,
    16#1F600 = (fun(<<_:2, X/little-utf16, _:6>>) -> X end)(?MODULE:id(LBin)),
    LBin = <<1:2, 16#1F600/little-utf16, 1:6>>,
    ok.

test_utf32() ->
    C = ?MODULE:id($a),
    <<128, 0, 0, 48, 1:1>> = <<1:1, C/utf32>>,
    97 = (fun(<<_:1, X/utf32>>) -> X end)(?MODULE:id(<<1:1, $a/utf32>>)),
    LBin = <<1:1, C/little-utf32>>,
    LBin = <<1:1, $a/little-utf32>>,
    97 = (fun(<<_:1, X/little-utf32>>) -> X end)(?MODULE:id(LBin)),
    ok.

%% Values in [2^63, 2^64) are bignums on a 64-bit VM but still fit a 64-bit
%% field.
test_uint64_bignum() ->
    H = ?MODULE:id(16#9E3779B97F4A7C15),
    <<207, 27, 188, 220, 191, 165, 62, 10, 1:1>> = <<1:1, H:64>>,
    Bin = <<1:1, H:64>>,
    H = (fun(<<_:1, X:64>>) -> X end)(?MODULE:id(Bin)),
    LBin = <<1:1, H:64/little>>,
    LBin = <<1:1, 16#9E3779B97F4A7C15:64/little>>,
    H = (fun(<<_:1, X:64/little>>) -> X end)(?MODULE:id(LBin)),
    Max = ?MODULE:id(16#FFFFFFFFFFFFFFFF),
    MaxBin = <<0:3, Max:64, 0:5>>,
    MaxBin = <<0:3, 16#FFFFFFFFFFFFFFFF:64, 0:5>>,
    ok.

test_float() ->
    F = ?MODULE:id(1.0),
    <<159, 192, 0, 0, 0:1>> = <<1:1, F:32/float>>,
    <<19, 248, 0, 0, 2>> = <<1:4, F:32/float, 2:4>>,
    1.0 = (fun(<<_:1, V:32/float>>) -> V end)(?MODULE:id(<<1:1, 1.0:32/float>>)),
    %% 16, 32 and 64 bits in both byte orders at every unaligned offset
    Values = ?MODULE:id([1.5, -2.25, 0.0, 65504.0, 3.0e-5]),
    ok = check_floats([16, 32, 64], Values),
    ok.

check_floats([], _Values) ->
    ok;
check_floats([Size | Sizes], Values) ->
    ok = check_floats(Size, 1, Values),
    check_floats(Sizes, Values).

check_floats(_Size, 8, _Values) ->
    ok;
check_floats(Size, Offset, Values) ->
    ok = check_floats_at(Size, Offset, Values),
    check_floats(Size, Offset + 1, Values).

check_floats_at(_Size, _Offset, []) ->
    ok;
check_floats_at(Size, Offset, [V | Values]) ->
    ok = check_float(Size, Offset, V),
    check_floats_at(Size, Offset, Values).

check_float(Size, Offset, V) ->
    Big = <<0:Offset, V:Size/float, 0:(8 - Offset)>>,
    Little = <<0:Offset, V:Size/little-float, 0:(8 - Offset)>>,
    %% the bits are those of the same float at a byte-aligned offset
    <<0:Offset, Bits:Size/bits, 0:(8 - Offset)>> = Big,
    Bits = <<V:Size/float>>,
    <<0:Offset, LBits:Size/bits, 0:(8 - Offset)>> = Little,
    LBits = <<V:Size/little-float>>,
    Expected = round_float(Size, V),
    Expected = (fun(<<_:Offset, X:Size/float, _/bits>>) -> X end)(?MODULE:id(Big)),
    Expected = (fun(<<_:Offset, X:Size/little-float, _/bits>>) -> X end)(?MODULE:id(Little)),
    ok.

round_float(Size, V) ->
    <<X:Size/float>> = <<V:Size/float>>,
    X.

-spec id(T) -> T.
id(X) ->
    X.
