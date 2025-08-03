%
% This file is part of AtomVM.
%
% Copyright 2024 Davide Bettio <davide@uninstall.it>
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

% WARNING: renaming this module will break the test
-module(test_fun_to_list).
-export([start/0, make_fun/0, make_fun/1, get_fun_bytes/0, fun_to_bin/1]).

start() ->
    <<"#Fun<test_fun_to_list.", Rest1/binary>> = ?MODULE:fun_to_bin(?MODULE:make_fun()),
    <<"#Fun<test_fun_to_list.", Rest2/binary>> = ?MODULE:fun_to_bin(?MODULE:make_fun(42)),

    [Id1, LastTok1] = binary:split(Rest1, <<".">>),
    [Uniq1, <<"">>] = binary:split(LastTok1, <<">">>),
    0 = erlang:binary_to_integer(Id1),
    Uniq = erlang:binary_to_integer(Uniq1),

    [Id2, LastTok2] = binary:split(Rest2, <<".">>),
    [Uniq2, <<"">>] = binary:split(LastTok2, <<">">>),

    1 = erlang:binary_to_integer(Id2),
    Uniq = erlang:binary_to_integer(Uniq2),

    <<"fun erlang:integer_to_list/1">> = ?MODULE:fun_to_bin(
        erlang:binary_to_term(erlang:list_to_binary(?MODULE:get_fun_bytes()))
    ),

    0.

make_fun() ->
    P = self(),
    fun() -> P ! foo end.

make_fun(N) ->
    P = self(),
    fun(X) -> P ! N + X end.

get_fun_bytes() ->
    [
        131,
        113,
        100,
        0,
        6,
        101,
        114,
        108,
        97,
        110,
        103,
        100,
        0,
        15,
        105,
        110,
        116,
        101,
        103,
        101,
        114,
        95,
        116,
        111,
        95,
        108,
        105,
        115,
        116,
        97,
        1
    ].

fun_to_bin(Fun) ->
    erlang:list_to_binary(erlang:fun_to_list(Fun)).
