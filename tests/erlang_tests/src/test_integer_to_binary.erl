%
% This file is part of AtomVM.
%
% Copyright 2019 Davide Bettio <davide@uninstall.it>
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

-module(test_integer_to_binary).

-export([start/0, some_calculation/2, compare_bin/3, id/1]).

start() ->
    ok = test_decimal(),
    ok = test_bases(),
    0.

test_decimal() ->
    NewBin1 = integer_to_binary(some_calculation(20, -2)),
    NewBin2 = integer_to_binary(some_calculation(1780, 0)),
    2 =
        compare_bin(NewBin1, <<"-1">>) + compare_bin(NewBin2, <<"89">>) -
            compare_bin(NewBin1, <<"+1">>) * 10 - compare_bin(NewBin2, <<"88">>) * 100,
    ok.

some_calculation(N, A) when is_integer(N) and is_integer(A) ->
    N div 20 + A;
some_calculation(_N, _A) ->
    -1.

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

test_bases() ->
    <<"0">> = integer_to_binary(?MODULE:id(0)),
    <<"0">> = integer_to_binary(?MODULE:id(0), 10),
    <<"-1">> = integer_to_binary(?MODULE:id(-1)),
    <<"-1010">> = integer_to_binary(?MODULE:id(-10), 2),
    <<"1010">> = integer_to_binary(?MODULE:id(10), 2),
    <<"A">> = integer_to_binary(?MODULE:id(10), 16),
    <<"123456789ABCDEF0">> = integer_to_binary(?MODULE:id(16#123456789ABCDEF0), 16),
    <<"7FFFFFFFFFFFFFFF">> = integer_to_binary(?MODULE:id(16#7FFFFFFFFFFFFFFF), 16),
    <<"-8000000000000000">> = integer_to_binary(?MODULE:id(-16#8000000000000000), 16),
    <<"A">> = integer_to_binary(?MODULE:id(10), 36),
    assert_badarg(fun() -> integer_to_binary(?MODULE:id(10), 1) end),
    assert_badarg(fun() -> integer_to_binary(?MODULE:id(10), 0) end),
    assert_badarg(fun() -> integer_to_binary(?MODULE:id(10), -1) end),
    assert_badarg(fun() -> integer_to_binary(?MODULE:id(10), 37) end),
    ok.

assert_badarg(F) ->
    ok =
        try
            F(),
            fail_no_ex
        catch
            error:badarg -> ok
        end.

id(I) -> I.
