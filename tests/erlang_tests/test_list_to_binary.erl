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

-export([start/0, concat_space/2, concat/2, is_binary_equal/3, id/1]).
-define(ID(X), ?MODULE:id(X)).
-define(ID(X1, X2), ?MODULE:id(X1), ?MODULE:id(X2)).

start() ->
    ok = test_list_to_binary(),
    ok = test_iolist_to_binary(),
    0.
test_list_to_binary() ->
    <<"Hello world">> = erlang:list_to_binary(?ID([<<"Hello ">>, [<<"wor">>, [$l, $d]]])),
    <<"">> = erlang:list_to_binary(?ID([])),

    % Concatenation
    Hello = concat_space(?ID("Hello", "world")),
    Empty = concat(?ID("", "")),
    0 = byte_size(Empty),
    true = is_binary_equal(Hello, <<"Hello world">>),
    false = is_binary_equal(Hello, <<"HelloXworld">>),

    % Errors
    ok = raises_badarg(fun() -> erlang:list_to_binary(?ID(<<>>)) end),
    ok = raises_badarg(fun() -> erlang:list_to_binary(?ID(<<"foo">>)) end),
    ok = raises_badarg(fun() -> erlang:list_to_binary(?ID(42)) end),
    ok.

test_iolist_to_binary() ->
    <<"foo">> = erlang:iolist_to_binary(?ID(<<"foo">>)),
    <<"">> = erlang:iolist_to_binary(?ID(<<>>)),
    <<"">> = erlang:iolist_to_binary(?ID([])),
    % The binary and charlist is "atom" in katakana as english transliteration ("atomu")
    % (in direct binary form since formatter butchers the utf-8)
    <<227, 130, 162, 227, 131, 136, 227, 131, 160>> = iolist_to_binary(
        ?ID(<<227, 130, 162, 227, 131, 136, 227, 131, 160>>)
    ),
    ok = raises_badarg(fun() -> iolist_to_binary(?ID([12450, 12488, 12512])) end),
    ok = raises_badarg(fun() -> iolist_size(?ID([12450, 12488, 12512])) end),
    ok.

concat_space(A, B) ->
    list_to_binary(A ++ " " ++ B).

concat(A, B) ->
    list_to_binary(A ++ B).

is_binary_equal(Bin1, Bin2) ->
    is_binary_equal(Bin1, Bin2, byte_size(Bin1) - 1).

is_binary_equal(_Bin1, _Bin2, -1) ->
    true;
is_binary_equal(Bin1, Bin2, Index) ->
    B1 = binary:at(Bin1, Index),
    case binary:at(Bin2, Index) of
        B1 ->
            is_binary_equal(Bin1, Bin2, Index - 1);
        _Any ->
            false
    end.

id(X) ->
    X.

raises_badarg(Fun) ->
    try Fun() of
        Ret -> {unexpected, Ret}
    catch
        error:badarg -> ok;
        C:E -> {unexpected, C, E}
    end.
