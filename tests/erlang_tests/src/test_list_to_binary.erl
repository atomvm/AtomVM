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

-module(test_list_to_binary).

-export([start/0, concat/2, concat2/2, compare_bin/3, id/1]).

start() ->
    ok = test_concat(),
    ok = test_iolist(),
    ok = test_iolist_to_binary(),
    ok = test_empty_list_to_binary(),
    0.

test_concat() ->
    Bin = concat("Hello", "world"),
    Bin2 = concat2("", ""),
    CompRes1 = compare_bin(Bin, <<"Hello world">>) - compare_bin(Bin, <<"HelloXworld">>),
    1 = CompRes1 + byte_size(Bin2) + invalid(42),
    ok.

test_iolist() ->
    <<"Hello world">> = list_to_binary(id([<<"Hello ">>, [<<"wor">>, [$l, $d]]])),
    ok.

test_iolist_to_binary() ->
    <<"Hello world">> = iolist_to_binary(id([<<"Hello ">>, [<<"wor">>, [$l, $d]]])),
    ok =
        try
            _ = list_to_binary(id(<<"foo">>)),
            fail
        catch
            error:badarg ->
                ok
        end,
    ok =
        try
            <<"foo">> = iolist_to_binary(id(<<"foo">>)),
            ok
        catch
            error:badarg ->
                fail
        end,
    ok.

test_empty_list_to_binary() ->
    <<"">> = erlang:list_to_binary([]),
    ok.

concat(A, B) ->
    list_to_binary(A ++ " " ++ B).

concat2(A, B) ->
    list_to_binary(A ++ B).

invalid(A) ->
    try list_to_binary(A) of
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
