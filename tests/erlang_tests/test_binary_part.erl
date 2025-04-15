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

-module(test_binary_part).

-export([start/0, id/1, fail_with_badarg/1, compare_bin/2, as_guard/4]).
-define(ID(Arg), ?MODULE:id(Arg)).

start() ->
    ok = test_with_binary_part_fun(?ID(fun binary:part/3)),
    ok = test_with_binary_part_fun(?ID(fun erlang:binary_part/3)),
    ok = test_with_binary_part_fun(
        ?ID(fun(Bin, Pos, Len) ->
            Pattern = binary_part(Bin, Pos, Len),
            ?MODULE:as_guard(Bin, Pattern, Pos, Len)
        end)
    ),
    0.

test_with_binary_part_fun(BinaryPart) ->
    Middle = BinaryPart(?ID(<<"012Testxyz">>), 3, 4),
    ok = compare_bin(Middle, <<"Test">>),
    First = BinaryPart(?ID(<<"First01234">>), 0, 5),
    ok = compare_bin(First, <<"First">>),
    Last = BinaryPart(?ID(<<"XYZLast">>), 3, 4),
    ok = compare_bin(Last, <<"Last">>),
    Empty = BinaryPart(?ID(<<"">>), 0, 0),
    0 = byte_size(Empty),
    All = BinaryPart(?ID(<<"01234">>), 0, 5),
    ok = compare_bin(All, <<"01234">>),
    NegativeCount = BinaryPart(?ID(<<"xyz">>), 1, -1),
    ok = compare_bin(NegativeCount, <<"x">>),
    ok = fail_with_badarg(fun() -> BinaryPart(?ID(<<"PastEnd">>), 0, 8) end),
    BadBinary = {0, 1, 2, 3, 4},
    ok = fail_with_badarg(fun() -> BinaryPart(?ID(BadBinary), 0, 1) end),
    ok = fail_with_badarg(fun() -> BinaryPart(?ID(BadBinary), 0, 0) end),
    ok = fail_with_badarg(fun() -> BinaryPart(?ID(<<"PosBeforeStart">>), -1, 0) end),
    ok = fail_with_badarg(fun() -> BinaryPart(?ID(<<"LenBeforeStart">>), 0, -1) end),
    ok = fail_with_badarg(fun() -> BinaryPart(?ID(<<"PartiallyBeforeStart">>), -1, 2) end),
    ok = fail_with_badarg(fun() -> BinaryPart(?ID(<<"BadPos">>), fail, 1) end),
    ok = fail_with_badarg(fun() -> BinaryPart(?ID(<<"BadLen">>), 1, fail) end),
    ok.

as_guard(Bin, Pattern, Pos, Len) when binary_part(Bin, Pos, Len) == Pattern ->
    Pattern;
as_guard(_Bin, _Pattern, _Pos, _Len) ->
    erlang:error(badarg).

id(X) ->
    X.

fail_with_badarg(Fun) ->
    try Fun() of
        Ret -> {unexpected, Ret}
    catch
        error:badarg -> ok;
        C:E -> {unexpected, C, E}
    end.

compare_bin(Bin1, Bin2) ->
    compare_bin(Bin1, Bin2, byte_size(Bin1) - 1).

compare_bin(_Bin1, _Bin2, -1) ->
    ok;
compare_bin(Bin1, Bin2, Index) ->
    B1 = binary:at(Bin1, Index),
    case binary:at(Bin2, Index) of
        B1 -> compare_bin(Bin1, Bin2, Index - 1);
        _Any -> error
    end.
