%
% This file is part of AtomVM.
%
% Copyright 2023 Davide Bettio <davide@uninstall.it>
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

-module(float_decode).
-export([
    start/0,
    decode/3,
    get_float/3,
    get_and_decode/3,
    zero_if_eq/2,
    zero_if_fails/1,
    inf/1,
    nan/1,
    inf_bin_fun/1,
    nan_bin_fun/1,
    decode_fun/1,
    compose/2
]).

start() ->
    test('-2.5', <<"-2.5">>) +
        test('512', <<"512.0">>) +
        test('0', <<"0.0">>) +
        zero_if_fails(?MODULE:nan(32)) + zero_if_fails(?MODULE:inf(32)) +
        zero_if_fails(?MODULE:nan(64)) + zero_if_fails(?MODULE:inf(64)).

test(Atom, Bin) ->
    F0 = binary_to_float(Bin),
    F1 = ?MODULE:get_and_decode(Atom, little, 32),
    F2 = ?MODULE:get_and_decode(Atom, big, 32),
    F3 = ?MODULE:get_and_decode(Atom, little, 64),
    F4 = ?MODULE:get_and_decode(Atom, big, 64),
    ?MODULE:zero_if_eq(F0, F1) + ?MODULE:zero_if_eq(F0, F3) +
        ?MODULE:zero_if_eq(F1, F2) + ?MODULE:zero_if_eq(F3, F4).

zero_if_eq(A, B) ->
    case A == B of
        true -> 0;
        _ -> 1
    end.

zero_if_fails(Fun) ->
    try
        Fun(),
        1
    catch
        error:{badmatch, Bin} when is_binary(Bin) -> 0;
        _:_ -> 100
    end.

get_and_decode(A, Endian, Size) ->
    ?MODULE:decode(?MODULE:get_float(A, Endian, Size), Endian, Size).

decode(Bin, little, 32) ->
    <<Val:32/little-float>> = Bin,
    Val;
decode(Bin, big, 32) ->
    <<Val:32/big-float>> = Bin,
    Val;
decode(Bin, little, 64) ->
    <<Val:64/little-float>> = Bin,
    Val;
decode(Bin, big, 64) ->
    <<Val:64/big-float>> = Bin,
    Val.

nan(Bits) ->
    ?MODULE:compose(?MODULE:decode_fun(Bits), ?MODULE:nan_bin_fun(Bits)).

inf(Bits) ->
    ?MODULE:compose(?MODULE:decode_fun(Bits), ?MODULE:inf_bin_fun(Bits)).

compose(A, B) ->
    fun() -> A(B()) end.

decode_fun(Bits) ->
    case Bits of
        32 -> fun(Bin) -> decode(Bin, little, 32) end;
        64 -> fun(Bin) -> decode(Bin, little, 64) end
    end.

nan_bin_fun(Bits) ->
    fun() ->
        case Bits of
            32 -> <<0, 0, 192, 127>>;
            64 -> <<0, 0, 0, 0, 0, 0, 248, 127>>
        end
    end.

inf_bin_fun(Bits) ->
    fun() ->
        case Bits of
            32 -> <<0, 0, 128, 127>>;
            64 -> <<0, 0, 0, 0, 0, 0, 240, 127>>
        end
    end.

get_float('-2.5', little, 32) ->
    <<0, 0, 32, 192>>;
get_float('-2.5', big, 32) ->
    <<192, 32, 0, 0>>;
get_float('-2.5', little, 64) ->
    <<0, 0, 0, 0, 0, 0, 4, 192>>;
get_float('-2.5', big, 64) ->
    <<192, 4, 0, 0, 0, 0, 0, 0>>;
get_float('512', little, 32) ->
    <<0, 0, 0, 68>>;
get_float('512', big, 32) ->
    <<68, 0, 0, 0>>;
get_float('512', little, 64) ->
    <<0, 0, 0, 0, 0, 0, 128, 64>>;
get_float('512', big, 64) ->
    <<64, 128, 0, 0, 0, 0, 0, 0>>;
get_float('0', little, 32) ->
    <<0, 0, 0, 0>>;
get_float('0', big, 32) ->
    <<0, 0, 0, 0>>;
get_float('0', little, 64) ->
    <<0, 0, 0, 0, 0, 0, 0, 0>>;
get_float('0', big, 64) ->
    <<0, 0, 0, 0, 0, 0, 0, 0>>.
