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

-module(test_binary_split).
-export([start/0, split_compare/3, split_compare/2, compare_bin/2, fail_split/1]).

start() ->
    split_compare(<<"Hello:World">>, <<"Hello">>, <<"World">>) +
    split_compare(<<"Hello:::World:">>, <<"Hello">>, <<"::World:">>) +
    split_compare(<<"Test:">>, <<"Test">>, <<>>) +
    split_compare(<<":">>, <<>>, <<>>) +
    split_compare(<<>>, <<>>) +
    split_compare(<<"Test">>, <<>>) +
    split_compare2(<<"Test">>, <<>>) +
    split_compare2(<<"helloSEPARATORworld">>, <<"hello">>, <<"world">>) +
    fail_split(<<>>) +
    fail_split({1, 2}) +
    fail_split2({1, 2}).

split_compare(Bin, Part1) ->
    [A] = binary:split(Bin, <<":">>),
    compare_bin(Part1, A).

split_compare(Bin, Part1, Part2) ->
    [A, B] = binary:split(Bin, <<":">>),
    compare_bin(Part1, A) + compare_bin(B, Part2).

split_compare2(Bin, Part1) ->
    [A] = binary:split(Bin, <<"SEPARATOR">>),
    compare_bin(Part1, A).

split_compare2(Bin, Part1, Part2) ->
    [A, B] = binary:split(Bin, <<"SEPARATOR">>),
    compare_bin(Part1, A) + compare_bin(B, Part2).

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

fail_split(Separator) ->
    try binary:split(<<"TESTBIN">>, Separator) of
        _Any -> 2000
    catch
        error:badarg -> 1;
        _:_ -> 4000
    end.

fail_split2(Bin) ->
    try binary:split(Bin, <<"TESTSEPARATOR">>) of
        _Any -> 2000
    catch
        error:badarg -> 1;
        _:_ -> 4000
    end.
