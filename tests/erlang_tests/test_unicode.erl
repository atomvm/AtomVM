%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_unicode).

-export([start/0]).

start() ->
    ok = test_to_list_latin1(),
    ok = test_to_list_utf8(),
    ok = test_to_binary_latin1(),
    ok = test_to_binary_utf8(),
    0.

test_to_list_latin1() ->
    "hello" = unicode:characters_to_list(<<"hello">>, latin1),
    "hello" = unicode:characters_to_list("hello", latin1),
    "hé" = unicode:characters_to_list(<<"hé">>, latin1),
    "hé" = unicode:characters_to_list(<<"hé">>, latin1),
    {error, "h", [-1]} = unicode:characters_to_list([$h, -1], latin1),
    {error, "h", [-1 | "ello"]} = unicode:characters_to_list([$h, -1 | "ello"], latin1),
    {error, "h", [16#110000]} = unicode:characters_to_list([$h, 16#110000], latin1),
    {error, "fooh", [[-1 | "ello"], "bar"]} = unicode:characters_to_list(
        ["foo", [$h, -1 | "ello"], "bar"], latin1
    ),
    ok =
        try
            unicode:characters_to_list([$h, self()], latin1),
            fail
        catch
            error:badarg -> ok
        end,
    ok.

test_to_list_utf8() ->
    "hello" = unicode:characters_to_list(<<"hello">>),
    "hello" = unicode:characters_to_list("hello", utf8),
    "hello" = unicode:characters_to_list("hello", unicode),
    "hé" = unicode:characters_to_list(<<"hé"/utf8>>),
    "hé" = unicode:characters_to_list("hé"),
    "hé" = unicode:characters_to_list(<<"hé"/utf8>>, utf8),
    "hé" = unicode:characters_to_list(<<"hé"/utf8>>, unicode),
    {error, "h", [-1]} = unicode:characters_to_list([$h, -1], utf8),
    {error, "h", [-1]} = unicode:characters_to_list([$h, -1], unicode),
    [$h, 16#10ffff] = unicode:characters_to_list([$h, 16#10ffff], utf8),
    [$h, 16#10ffff] = unicode:characters_to_list([$h, 16#10ffff], unicode),
    {error, "h", [16#110000]} = unicode:characters_to_list([$h, 16#110000], utf8),
    {error, "h", [16#110000]} = unicode:characters_to_list([$h, 16#110000], unicode),
    {incomplete, "h", <<"é">>} = unicode:characters_to_list(<<"hé">>),
    {error, [], <<16#A0, 16#A1>>} = unicode:characters_to_list(<<16#A0, 16#A1>>),
    % Erlang/OTP documentation writes: "The last part is mostly for debugging"
    % BEAM and ATOM representation differ a little bit
    Expected1 =
        case erlang:system_info(machine) of
            "BEAM" -> [<<"é">>, [[["bar"]]]];
            "ATOM" -> [[<<"é">>, ["bar"]]]
        end,
    {error, "fooh", Expected1} = unicode:characters_to_list(["foo", [<<"hé">>, ["bar"]]]),
    Expected2 =
        case erlang:system_info(machine) of
            "BEAM" -> [<<"é">>, [[["bar"], "foobar"]]];
            "ATOM" -> [[<<"é">>, ["bar"], "foobar"]]
        end,
    {error, "fooh", Expected2} = unicode:characters_to_list(["foo", [<<"hé">>, ["bar"], "foobar"]]),
    ok.

test_to_binary_latin1() ->
    <<"hello">> = unicode:characters_to_binary("hello", latin1, latin1),
    <<"hello">> = unicode:characters_to_binary(<<"hello">>, latin1, latin1),
    <<"hé">> = unicode:characters_to_binary("hé", latin1, latin1),
    <<"hé">> = unicode:characters_to_binary(<<"hé"/utf8>>, utf8, latin1),
    % For some reason, Erlang/OTP fails on -1 for latin1
    ok =
        case erlang:system_info(machine) of
            "BEAM" ->
                try
                    unicode:characters_to_binary([$h, -1], latin1, latin1)
                catch
                    error:badarg -> ok
                end;
            "ATOM" ->
                {error, <<"h">>, [-1]} = unicode:characters_to_binary([$h, -1], latin1, latin1),
                ok
        end,
    Expected0 =
        case erlang:system_info(machine) of
            "BEAM" -> [[16#110000]];
            "ATOM" -> [16#110000]
        end,
    {error, <<"h">>, Expected0} = unicode:characters_to_binary([$h, 16#110000], latin1, latin1),
    Expected1 =
        case erlang:system_info(machine) of
            "BEAM" -> [[2000]];
            "ATOM" -> [2000]
        end,
    {error, <<"h">>, Expected1} = unicode:characters_to_binary([$h, 2000], latin1, latin1),
    Expected2 =
        case erlang:system_info(machine) of
            "BEAM" -> [[2000] | "ello"];
            "ATOM" -> [2000 | "ello"]
        end,
    {error, <<"h">>, Expected2} = unicode:characters_to_binary([$h, 2000 | "ello"], latin1, latin1),
    Expected3 =
        case erlang:system_info(machine) of
            "BEAM" -> [[[2000] | "ello"], "bar"];
            "ATOM" -> [[2000 | "ello"], "bar"]
        end,
    {error, <<"fooh">>, Expected3} = unicode:characters_to_binary(
        ["foo", [$h, 2000 | "ello"], "bar"], latin1, latin1
    ),
    ok =
        try
            unicode:characters_to_binary([$h, self()], latin1, latin1),
            fail
        catch
            error:badarg -> ok
        end,
    ok.

test_to_binary_utf8() ->
    <<"hello">> = unicode:characters_to_binary("hello", utf8, utf8),
    <<"hello">> = unicode:characters_to_binary("hello", unicode, unicode),
    <<"hello">> = unicode:characters_to_binary("hello", utf8, unicode),
    <<"hello">> = unicode:characters_to_binary("hello", unicode, utf8),
    <<"hello">> = unicode:characters_to_binary(<<"hello">>, utf8, utf8),
    <<"hello">> = unicode:characters_to_binary(<<"hello">>, unicode, unicode),
    <<"hello">> = unicode:characters_to_binary(<<"hello">>, utf8, unicode),
    <<"hello">> = unicode:characters_to_binary(<<"hello">>, unicode, utf8),
    <<"hé"/utf8>> = unicode:characters_to_binary("hé", latin1, utf8),
    <<"hé"/utf8>> = unicode:characters_to_binary("hé", latin1, unicode),
    <<"hé"/utf8>> = unicode:characters_to_binary(<<"hé"/utf8>>, utf8, utf8),
    <<"hé"/utf8>> = unicode:characters_to_binary(<<"hé"/utf8>>, unicode, unicode),
    <<"hé"/utf8>> = unicode:characters_to_binary(<<"hé"/utf8>>, utf8, unicode),
    <<"hé"/utf8>> = unicode:characters_to_binary(<<"hé"/utf8>>, unicode, utf8),
    <<"hé"/utf8>> = unicode:characters_to_binary(<<"hé"/utf8>>, utf8),
    <<"hé"/utf8>> = unicode:characters_to_binary(<<"hé"/utf8>>, unicode),
    <<"hé"/utf8>> = unicode:characters_to_binary(<<"hé"/utf8>>),
    {error, <<"h">>, [-1]} = unicode:characters_to_binary([$h, -1]),
    <<"h", 244, 143, 191, 191>> = unicode:characters_to_binary([$h, 16#10FFFF]),
    {error, <<"h">>, [16#110000]} = unicode:characters_to_binary([$h, 16#110000]),
    {incomplete, <<"h">>, <<"é">>} = unicode:characters_to_binary(<<"hé">>),
    {error, <<>>, <<16#A0, 16#A1>>} = unicode:characters_to_binary(<<16#A0, 16#A1>>),
    Expected1 =
        case erlang:system_info(machine) of
            "BEAM" -> [<<"é">>, [[["bar"]]]];
            "ATOM" -> [[<<"é">>, ["bar"]]]
        end,
    {error, <<"fooh">>, Expected1} = unicode:characters_to_binary(["foo", [<<"hé">>, ["bar"]]]),
    Expected2 =
        case erlang:system_info(machine) of
            "BEAM" -> [<<"é">>, [[["bar"], "foobar"]]];
            "ATOM" -> [[<<"é">>, ["bar"], "foobar"]]
        end,
    {error, <<"fooh">>, Expected2} = unicode:characters_to_binary([
        "foo", [<<"hé">>, ["bar"], "foobar"]
    ]),
    ok.
