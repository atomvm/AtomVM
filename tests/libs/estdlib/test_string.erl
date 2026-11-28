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
    ok = test_to_lower(),
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
    ?ASSERT_MATCH(string:to_upper($a), $A),
    ?ASSERT_MATCH(string:to_upper(233), 201),
    ?ASSERT_MATCH(string:to_upper(-1), -1),
    ?ASSERT_MATCH(string:to_upper([224, 233, 246, 247, 248, 254, 255]), [
        192, 201, 214, 247, 216, 222, 255
    ]),
    ?ASSERT_MATCH(string:to_upper([256, 8364, 128512]), [256, 8364, 128512]),
    ?ASSERT_MATCH(string:to_upper([$a, bad]), [$A, bad]),
    ok.

test_to_lower() ->
    ?ASSERT_MATCH(string:to_lower(""), ""),
    ?ASSERT_MATCH(string:to_lower("aBc123!Z"), "abc123!z"),
    ?ASSERT_MATCH(string:to_lower($A), $a),
    ?ASSERT_MATCH(string:to_lower(201), 233),
    ?ASSERT_MATCH(string:to_lower(-1), -1),
    ?ASSERT_MATCH(string:to_lower([192, 201, 214, 215, 216, 222, 223]), [
        224, 233, 246, 215, 248, 254, 223
    ]),
    ?ASSERT_MATCH(string:to_lower([256, 8364, 128512]), [256, 8364, 128512]),
    ?ASSERT_MATCH(string:to_lower([$A, bad]), [$a, bad]),
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
    ?ASSERT_MATCH(string:split("ab..bc..cd", "..", trailing), ["ab..bc", "cd"]),
    ?ASSERT_MATCH(string:split(<<"ab..bc..cd">>, ".."), [<<"ab">>, <<"bc..cd">>]),
    ?ASSERT_MATCH(string:split(<<"ab..bc..cd">>, "..", leading), [<<"ab">>, <<"bc..cd">>]),
    ?ASSERT_MATCH(string:split(<<"ab..bc..cd">>, "..", trailing), [<<"ab..bc">>, <<"cd">>]),
    ?ASSERT_MATCH(string:split(<<"ab..bc..cd">>, "..", all), [<<"ab">>, <<"bc">>, <<"cd">>]),

    ?ASSERT_MATCH(string:split("ab..bc..cd", <<"..">>), ["ab", "bc..cd"]),
    ?ASSERT_MATCH(string:split(<<"ab..bc..cd">>, <<"..">>, leading), [<<"ab">>, <<"bc..cd">>]),
    ?ASSERT_MATCH(string:split(<<"ab..bc..cd">>, <<"..">>, trailing), [<<"ab..bc">>, <<"cd">>]),
    ?ASSERT_MATCH(string:split(<<"ab..bc..cd">>, <<"..">>, all), [<<"ab">>, <<"bc">>, <<"cd">>]),

    ?ASSERT_MATCH(string:split("aaa", "aa", trailing), ["a", ""]),
    ?ASSERT_MATCH(string:split("aaaaa", "aa", trailing), ["aaa", ""]),
    ?ASSERT_MATCH(string:split("aaaaa", "aa", all), ["", "", "a"]),
    ?ASSERT_MATCH(string:split(<<"aaa">>, <<"aa">>, trailing), [<<"a">>, <<>>]),
    ?ASSERT_MATCH(string:split("a", "a", trailing), ["", ""]),
    ?ASSERT_MATCH(string:split("abc", "", leading), ["abc"]),
    ?ASSERT_MATCH(string:split("abc", "", trailing), ["abc"]),
    ?ASSERT_MATCH(string:split("abc", "", all), ["abc"]),
    ?ASSERT_MATCH(string:split(<<"abc">>, <<>>, leading), [<<"abc">>]),
    ?ASSERT_MATCH(string:split(<<"abc">>, <<>>, trailing), [<<"abc">>]),
    ?ASSERT_MATCH(string:split(<<"abc">>, <<>>, all), [<<"abc">>]),
    ?ASSERT_MATCH(string:split(<<"aé—bé—c"/utf8>>, "é—", trailing), [<<"aé—b"/utf8>>, <<"c">>]),
    ?ASSERT_MATCH(string:split("aé—bé—c", <<"é—"/utf8>>, trailing), ["aé—b", "c"]),

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
    ?ASSERT_MATCH(string:trim("\t\n\r\f\v hi \r\n\t"), "hi"),
    ?ASSERT_MATCH(string:trim([133, 8206, 8207, 8232, 8233, $a, 8233, 133]), "a"),
    ?ASSERT_MATCH(string:trim([160, $a, 160]), [160, $a, 160]),
    ?ASSERT_MATCH(string:trim(<<"  hi  ">>), <<"hi">>),
    ?ASSERT_MATCH(string:trim(<<"\t hi \n">>, leading), <<"hi \n">>),
    ?ASSERT_MATCH(string:trim(<<"\t hi \n">>, trailing), <<"\t hi">>),
    ?ASSERT_MATCH(string:trim(<<"\t hi \n">>, both), <<"hi">>),
    ?ASSERT_MATCH(string:trim(<<" \t\r\n ">>), <<>>),
    ?ASSERT_MATCH(string:trim(<<" \t\r\né\n\t "/utf8>>), <<"é"/utf8>>),
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

    ?ASSERT_MATCH(string:find("ab..bc..cd", "..", trailing), "..cd"),
    ?ASSERT_MATCH(string:find(<<"ab..bc..cd">>, "..", trailing), <<"..cd">>),
    ?ASSERT_MATCH(string:find("aaa", "aa", trailing), "aa"),
    ?ASSERT_MATCH(string:find(<<"aaa">>, <<"aa">>, trailing), <<"aa">>),
    ?ASSERT_MATCH(string:find("abc", "z", trailing), nomatch),
    ?ASSERT_MATCH(string:find(<<"abc">>, <<"z">>, trailing), nomatch),
    ?ASSERT_MATCH(string:find("abc", "", trailing), "abc"),
    ?ASSERT_MATCH(string:find(<<"abc">>, <<>>, trailing), <<"abc">>),
    ?ASSERT_MATCH(string:find(<<"aé—bé—c"/utf8>>, "é—", trailing), <<"é—c"/utf8>>),

    ok.

test_length() ->
    ?ASSERT_MATCH(string:length(""), 0),
    ?ASSERT_MATCH(string:length(<<>>), 0),
    ?ASSERT_MATCH(string:length("foo"), 3),
    ?ASSERT_MATCH(string:length(<<"foo">>), 3),
    ?ASSERT_MATCH(string:length("アトム"), 3),
    ?ASSERT_MATCH(string:length(<<"アトム"/utf8>>), 3),
    ?ASSERT_MATCH(string:length(["ab", <<"cd">>]), 4),
    ?ASSERT_MATCH(string:length([$a | <<"bc">>]), 3),

    ok.

id(X) -> X.
