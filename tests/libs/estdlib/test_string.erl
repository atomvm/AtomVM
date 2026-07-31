%
% This file is part of AtomVM.
%
% Copyright 2020 Fred Dushin <fred@dushin.net>
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

-module(test_string).

-export([test/0, id/1]).

-include("etest.hrl").

test() ->
    ok = test_to_upper(),
    ok = test_split(),
    ok = test_trim(),
    ok = test_find(),
    ok = test_length(),
    ok = test_to_integer(),
    ok.

test_to_integer() ->
    {27, ""} = string:to_integer("27"),
    {12, "abc"} = string:to_integer("12abc"),
    {-5, "x"} = string:to_integer("-5x"),
    {42, ""} = string:to_integer("+42"),
    {1, "+2"} = string:to_integer("0001+2"),
    {1, <<"+2">>} = string:to_integer(<<"1+2">>),
    {123, <<"x">>} = string:to_integer(<<"123x">>),
    {-5, <<"x">>} = string:to_integer(<<"-5x">>),
    {12, <<"é"/utf8>>} = string:to_integer(<<"12é"/utf8>>),
    {12, <<"x">>} = string:to_integer([$1 | <<"2x">>]),
    {123, "x"} = string:to_integer([[<<"12">>], "3x"]),
    {12, <<"x">>} = string:to_integer([[], <<"12x">>]),
    {12, []} = string:to_integer([<<"12">>, [], []]),
    {error, no_integer} = string:to_integer("abc"),
    {error, no_integer} = string:to_integer(""),
    {error, no_integer} = string:to_integer(<<"+">>),
    {error, no_integer} = string:to_integer("+"),
    {error, no_integer} = string:to_integer("--1"),
    {error, no_integer} = string:to_integer("++1"),
    {error, no_integer} = string:to_integer([<<"abc">>, 16#FFFFF]),
    {error, no_integer} = string:to_integer([[], [<<>>], []]),
    {error, no_integer} = string:to_integer([[$-], [], <<>>]),
    %% Valid first nonnumeric character: malformed tail is unexamined.
    {error, no_integer} = string:to_integer(<<"a", 16#FF>>),
    {error, no_integer} = string:to_integer([<<"abc">>, <<16#FF>>]),
    %% Boundary char outside take-set: leftover signs are not validated past it.
    {1, <<$x, 16#FF>>} = string:to_integer(<<$1, $x, 16#FF>>),
    {1, [<<"x">>, <<16#FF>>]} = string:to_integer([<<"1x">>, <<16#FF>>]),
    {error, badarg} = string:to_integer(foo),
    {error, badarg} = string:to_integer([$3, hello]),
    {error, badarg} = string:to_integer([49, 50 | bad]),
    {error, badarg} = string:to_integer(<<$3, 16#FF>>),
    {error, badarg} = string:to_integer([<<"12">>, <<255>>]),
    %% Malformed first codepoint (or after sign) is badarg, not no_integer.
    {error, badarg} = string:to_integer(<<16#FF>>),
    {error, badarg} = string:to_integer(<<16#C2>>),
    {error, badarg} = string:to_integer(<<$+, 16#FF>>),
    {error, badarg} = string:to_integer([16#110000]),
    {error, badarg} = string:to_integer([$1, 16#110000]),
    %% Leftover signs in the take-set prefix are validated (OTP take semantics).
    {error, badarg} = string:to_integer(<<$1, $+, 16#FF>>),
    {error, badarg} = string:to_integer([$1, $+, <<16#FF>>]),
    {error, badarg} = string:to_integer(<<$-, $-, 16#FF>>),
    {error, badarg} = string:to_integer([<<"1+">>, <<16#FF>>]),
    %% Improper list spines are rejected even when behind a valid boundary.
    {error, badarg} = string:to_integer([$a | foo]),
    {error, badarg} = string:to_integer([$1, $x | foo]),
    {error, badarg} = string:to_integer([<<"1x">> | foo]),
    {error, badarg} = string:to_integer([$1 | foo]),
    %% Large integer near AtomVM's ~256-bit magnitude limit (supported on both).
    BigOk = lists:duplicate(77, $9),
    BigOkBin = list_to_binary(BigOk),
    {BigOkInt, []} = string:to_integer(BigOk),
    true = is_integer(BigOkInt) andalso BigOkInt > 0,
    {BigOkInt, <<>>} = string:to_integer(BigOkBin),
    %% Oversized for AtomVM (~272 bits); OTP accepts arbitrary size.
    BigOver = lists:duplicate(80, $9),
    BigOverBin = list_to_binary(BigOver),
    case erlang:system_info(machine) of
        "BEAM" ->
            {BigOverInt, []} = string:to_integer(BigOver),
            true = is_integer(BigOverInt) andalso BigOverInt > BigOkInt,
            {BigOverInt, <<>>} = string:to_integer(BigOverBin),
            ok;
        _ ->
            {error, badarg} = string:to_integer(BigOver),
            {error, badarg} = string:to_integer(BigOverBin),
            ok
    end,
    ok.

test_to_upper() ->
    ?ASSERT_MATCH(string:to_upper(""), ""),
    ?ASSERT_MATCH(string:to_upper("abc"), "ABC"),
    ?ASSERT_MATCH(string:to_upper("aBc"), "ABC"),
    ?ASSERT_MATCH(string:to_upper("aBc123%#$x5"), "ABC123%#$X5"),
    ok.

test_split() ->
    ?ASSERT_MATCH(string:split("", " "), [""]),
    ?ASSERT_MATCH(string:split("foo bar", " "), ["foo", "bar"]),
    ?ASSERT_MATCH(string:split("foo bar   tapas", " "), ["foo", "bar   tapas"]),
    ?ASSERT_MATCH(string:split("foo bar   tapas", " ", all), ["foo", "bar", [], [], "tapas"]),

    ?ASSERT_MATCH(string:split("foo bar", "XXX"), ["foo bar"]),
    ?ASSERT_MATCH(string:split("fooXXXbar", "XXX"), ["foo", "bar"]),
    ?ASSERT_MATCH(string:split("foo barXXXtapas", "XXX"), ["foo bar", "tapas"]),
    ?ASSERT_MATCH(string:split("foo barXXXXXXtapas", "XXX", all), ["foo bar", [], "tapas"]),

    ?ASSERT_MATCH(string:split("ab..bc..cd", ".."), ["ab", "bc..cd"]),
    ?ASSERT_MATCH(string:split(<<"ab..bc..cd">>, ".."), [<<"ab">>, <<"bc..cd">>]),
    ?ASSERT_MATCH(string:split(<<"ab..bc..cd">>, "..", leading), [<<"ab">>, <<"bc..cd">>]),
    %   ?ASSERT_MATCH(string:split(<<"ab..bc..cd">>, "..", trailing), [<<"ab..bc">>, <<"cd">>]),
    ?ASSERT_MATCH(string:split(<<"ab..bc..cd">>, "..", all), [<<"ab">>, <<"bc">>, <<"cd">>]),

    ?ASSERT_MATCH(string:split("ab..bc..cd", <<"..">>), ["ab", "bc..cd"]),
    ?ASSERT_MATCH(string:split(<<"ab..bc..cd">>, <<"..">>, leading), [<<"ab">>, <<"bc..cd">>]),
    %   ?ASSERT_MATCH(string:split(<<"ab..bc..cd">>, <<"..">>, trailing), [<<"ab..bc">>, <<"cd">>]),
    ?ASSERT_MATCH(string:split(<<"ab..bc..cd">>, <<"..">>, all), [<<"ab">>, <<"bc">>, <<"cd">>]),

    ok.

test_trim() ->
    ?ASSERT_MATCH(string:trim(""), ""),
    ?ASSERT_MATCH(string:trim(" foo bar"), "foo bar"),
    ?ASSERT_MATCH(string:trim(" foo bar", leading), "foo bar"),
    ?ASSERT_MATCH(string:trim(" foo bar", trailing), " foo bar"),
    ?ASSERT_MATCH(string:trim(" foo bar ", trailing), " foo bar"),
    ?ASSERT_MATCH(string:trim(" foo bar      ", trailing), " foo bar"),
    ?ASSERT_MATCH(string:trim(" foo bar ", both), "foo bar"),
    ?ASSERT_MATCH(string:trim("      foo bar      ", both), "foo bar"),
    ok.

test_find() ->
    ?ASSERT_MATCH(string:find("", ""), ""),
    ?ASSERT_MATCH(string:find("foo", ""), "foo"),
    ?ASSERT_MATCH(string:find("", "foo"), nomatch),
    ?ASSERT_MATCH(string:find(<<>>, <<>>), <<>>),
    ?ASSERT_MATCH(string:find(<<>>, ""), <<>>),
    ?ASSERT_MATCH(string:find(<<"foo">>, <<"">>), <<"foo">>),
    ?ASSERT_MATCH(string:find(<<"foo">>, ""), <<"foo">>),
    ?ASSERT_MATCH(string:find(<<"">>, <<"foo">>), nomatch),
    ?ASSERT_MATCH(string:find(<<"">>, "foo"), nomatch),

    ?ASSERT_MATCH(string:find("foobar", "ba"), "bar"),
    ?ASSERT_MATCH(string:find(<<"foobar">>, "ba"), <<"bar">>),
    ?ASSERT_MATCH(string:find("foobar", <<"ba">>), "bar"),
    ?ASSERT_MATCH(string:find(<<"foobar">>, <<"ba">>), <<"bar">>),

    ok.

test_length() ->
    ?ASSERT_MATCH(string:length(""), 0),
    ?ASSERT_MATCH(string:length(<<>>), 0),
    ?ASSERT_MATCH(string:length("foo"), 3),
    ?ASSERT_MATCH(string:length(<<"foo">>), 3),
    ?ASSERT_MATCH(string:length("アトム"), 3),
    ?ASSERT_MATCH(string:length(<<"アトム"/utf8>>), 3),

    ok.

id(X) -> X.
