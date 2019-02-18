-module(test_proplists).

-export([test/0]).

-include("estdlib.hrl").

test() ->
    ok = test_get_value(),
    ok.

test_get_value() ->
    ok = etest:assert_match(?PROPLISTS:get_value(a, []), undefined),
    ok = etest:assert_match(?PROPLISTS:get_value(a, [a]), true),
    ok = etest:assert_match(?PROPLISTS:get_value(a, [{a, foo}]), foo),

    ok = etest:assert_match(?PROPLISTS:get_value(a, [], gnu), gnu),
    ok = etest:assert_match(?PROPLISTS:get_value(a, [a], gnu), true),
    ok = etest:assert_match(?PROPLISTS:get_value(a, [{a, foo}], gnu), foo),
    ok = etest:assert_match(?PROPLISTS:get_value(b, [{a, foo}], gnu), gnu),
    ok.
