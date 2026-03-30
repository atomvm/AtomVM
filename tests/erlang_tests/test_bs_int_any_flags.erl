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

%% @doc Test bitstring insert/match for non-standard integer sizes (e.g. 24, 48)
%% with endianness and signedness flags. These sizes go through
%% bitstring_insert_any_integer which must handle flags correctly.
-module(test_bs_int_any_flags).

-export([start/0]).

start() ->
    ok = test_24bit_little_unsigned(id(16#123456)),
    ok = test_24bit_little_signed(id(-1), id(-256)),
    ok = test_24bit_big_unsigned(id(16#123456)),
    ok = test_24bit_roundtrip(id(16#ABCDEF), id(-12345)),
    ok = test_48bit_little_unsigned(id(16#112233445566)),
    ok = test_48bit_roundtrip(id(16#AABBCCDDEEFF)),
    ok = test_24bit_little_unaligned(id(16#123456)),
    0.

test_24bit_little_unsigned(Val) ->
    Bin = <<Val:24/little>>,
    <<16#56, 16#34, 16#12>> = Bin,
    ok.

test_24bit_little_signed(V1, V2) ->
    Bin = <<V1:24/little-signed>>,
    <<16#FF, 16#FF, 16#FF>> = Bin,
    Bin2 = <<V2:24/little-signed>>,
    <<16#00, 16#FF, 16#FF>> = Bin2,
    ok.

test_24bit_big_unsigned(Val) ->
    %% Big-endian non-standard size (should already work, sanity check)
    Bin = <<Val:24/big>>,
    <<16#12, 16#34, 16#56>> = Bin,
    ok.

test_24bit_roundtrip(Val, NegVal) ->
    Bin = <<Val:24/little-unsigned>>,
    <<Got:24/little-unsigned>> = Bin,
    Val = Got,
    BinS = <<NegVal:24/little-signed>>,
    <<GotS:24/little-signed>> = BinS,
    NegVal = GotS,
    ok.

test_48bit_little_unsigned(Val) ->
    Bin = <<Val:48/little>>,
    <<16#66, 16#55, 16#44, 16#33, 16#22, 16#11>> = Bin,
    ok.

test_48bit_roundtrip(Val) ->
    Bin = <<Val:48/little-unsigned>>,
    <<Got:48/little-unsigned>> = Bin,
    Val = Got,
    ok.

test_24bit_little_unaligned(Val) ->
    %% Offset is 1 bit, not byte-aligned — exercises the byte-swap fallback path
    Bin = <<1:1, Val:24/little, 0:7>>,
    <<1:1, Got:24/little, 0:7>> = Bin,
    Val = Got,
    ok.

id(X) -> X.
