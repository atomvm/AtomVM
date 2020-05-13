-module(test_string).

-export([test/0, id/1]).

-include("etest.hrl").

test() ->
    ok = test_to_upper(),
    ok = test_split(),
    ok = test_trim(),
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


id(X) -> X.
