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
