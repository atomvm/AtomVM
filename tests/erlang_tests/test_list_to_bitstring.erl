%
% This file is part of AtomVM.
%
% Copyright 2025 Franciszek Kubis <franciszek.kubis@swmansion.com>
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

-module(test_list_to_bitstring).

-export([start/0, concat/2, concat2/2, compare_bin/3, id/1]).

start() ->
    ok = test_concat(),
    ok = test_iolist(),
    ok = test_empty_list_to_binary(),
    ok = test_bit_level(),
    ok = test_large_elements(),
    0.

test_concat() ->
    Bin = concat("Hello", "world"),
    Bin2 = concat2("", ""),
    CompRes1 = compare_bin(Bin, <<"Hello world">>) - compare_bin(Bin, <<"HelloXworld">>),
    1 = CompRes1 + byte_size(Bin2) + invalid(42),
    ok.

test_iolist() ->
    <<"Hello world">> = list_to_bitstring(?MODULE:id([<<"Hello ">>, [<<"wor">>, [$l, $d]]])),
    ok.

test_empty_list_to_binary() ->
    <<"">> = erlang:list_to_bitstring(?MODULE:id([])),
    ok.

test_bit_level() ->
    <<2:2>> = list_to_bitstring(?MODULE:id([<<1:1>>, <<0:1>>])),
    % improper tail may be a bitstring
    <<2:2>> = list_to_bitstring(?MODULE:id([<<1:1>> | <<0:1>>])),
    <<15:4>> = list_to_bitstring(?MODULE:id([[<<1:1>> | <<7:3>>]])),
    % mixed bytes and bit fragments, nested
    <<"ab", 1:3, 0:5>> = list_to_bitstring(?MODULE:id([<<"ab">>, [<<1:3>>], <<0:5>>])),
    <<65, 1:1, 66:8, 3:7, 67>> = list_to_bitstring(?MODULE:id([65, <<1:1>>, 66, <<3:7>>, 67])),
    3000 = bit_size(list_to_bitstring(?MODULE:id(duplicate(1000, <<5:3>>, [])))),
    % non-bitstring elements are rejected
    0 = invalid([<<1:1>>, 3.14]),
    0 = invalid([<<1:1>> | some_atom]),
    0 = invalid([256]),
    % bytes are only accepted as elements: an improper tail may be a bitstring
    % but never an integer
    <<1, 2>> = list_to_bitstring(?MODULE:id([1 | <<2>>])),
    <<1, 2, 1:1>> = list_to_bitstring(?MODULE:id([[1 | <<2>>] | <<1:1>>])),
    <<"">> = list_to_bitstring(?MODULE:id([[]])),
    0 = invalid([1 | 2]),
    0 = invalid([<<1:1>> | 2]),
    0 = invalid([[1 | 2]]),
    0 = invalid([1, 2 | 3]),
    ok.

% Elements large enough that the result is allocated as a refc binary rather
% than a heap binary, including the same binary referenced several times.
test_large_elements() ->
    Big = ?MODULE:id(seq_binary(100, <<>>)),
    100 = byte_size(Big),
    Twice = list_to_bitstring(?MODULE:id([Big, Big])),
    200 = byte_size(Twice),
    <<Big:100/binary, Big:100/binary>> = Twice,
    % same, with a trailing partial element: the whole bytes are copied into a
    % refc binary that gets wrapped in a sub-binary
    Odd = list_to_bitstring(?MODULE:id([Big, Big, <<1:1>>])),
    1601 = bit_size(Odd),
    false = is_binary(Odd),
    <<Big:100/binary, Big:100/binary, 1:1>> = Odd,
    % a leading partial element shifts every following byte
    Shifted = list_to_bitstring(?MODULE:id([<<0:1>>, Big])),
    801 = bit_size(Shifted),
    <<0:1, Big:100/binary>> = Shifted,
    ok.

seq_binary(0, Acc) -> Acc;
seq_binary(N, Acc) -> seq_binary(N - 1, <<(N - 1):8, Acc/binary>>).

duplicate(0, _T, Acc) -> Acc;
duplicate(N, T, Acc) -> duplicate(N - 1, T, [T | Acc]).

concat(A, B) ->
    list_to_bitstring(?MODULE:id(A ++ " " ++ B)).

concat2(A, B) ->
    list_to_bitstring(?MODULE:id(A ++ B)).

invalid(A) ->
    try list_to_bitstring(?MODULE:id(A)) of
        Any -> byte_size(Any)
    catch
        error:badarg -> 0;
        _:_ -> 1000
    end.

compare_bin(Bin1, Bin2) ->
    compare_bin(Bin1, Bin2, byte_size(Bin1) - 1).

compare_bin(_Bin1, _Bin2, -1) ->
    1;
compare_bin(Bin1, Bin2, Index) ->
    B1 = binary:at(Bin1, Index),
    case binary:at(Bin2, Index) of
        B1 ->
            compare_bin(Bin1, Bin2, Index - 1);
        _Any ->
            0
    end.

id(X) ->
    X.
