-module(test_lists).

-export([test/0, id/1]).

-include("etest.hrl").

test() ->
    ok = test_nth(),
    ok = test_member(),
    ok = test_reverse(),
    ok = test_keyfind(),
    ok = test_keydelete(),
    ok = test_list_match(),
    ok.

test_nth() ->
    ?ASSERT_MATCH(lists:nth(1, [a,b,c]), a),
    ?ASSERT_MATCH(lists:nth(2, [a,b,c]), b),
    ?ASSERT_MATCH(lists:nth(3, [a,b,c]), c),
    % try
    %     lists:nth(-1, [a,b,c]),
    %     throw(failure)
    % catch
    %     _:_ -> ok
    % end,
    ok.

test_reverse() ->
    ?ASSERT_MATCH(lists:reverse([]), []),
    ?ASSERT_MATCH(lists:reverse([a]), [a]),
    ?ASSERT_MATCH(lists:reverse([a, b]), [b,a]),

    ok.

test_member() ->
    ?ASSERT_TRUE(lists:member(a, [a,b,c])),
    ?ASSERT_TRUE(lists:member(b, [a,b,c])),
    ?ASSERT_TRUE(lists:member(c, [a,b,c])),
    ?ASSERT_TRUE(not lists:member(d, [a,b,c])),
    ?ASSERT_TRUE(not lists:member(x, [])),
    ok.

test_keyfind() ->
    ?ASSERT_MATCH(lists:keyfind(a, 1, [{a, x}, b, []]), {a, x}),
    ?ASSERT_MATCH(lists:keyfind(x, 2, [{a, x}, b, []]), {a, x}),
    ?ASSERT_MATCH(lists:keyfind(x, 3, [{a, x}, b, []]), false),
    ?ASSERT_MATCH(lists:keyfind(b, 1, [{a, 1}, b, []]), false),

    ?ASSERT_MATCH(lists:keyfind(b, 1, [{a, x}, {b, foo, bar}, []]), {b, foo, bar}),
    ?ASSERT_MATCH(lists:keyfind(nope, 2, [{a, x}, {b, foo, bar}, []]), false),
    ok.

test_keydelete() ->
    ?ASSERT_MATCH(lists:keydelete(a, 1, []), []),
    ?ASSERT_MATCH(lists:keydelete(a, 1, [{a, x}, b, []]), [b, []]),
    ?ASSERT_MATCH(lists:keydelete(a, 1, [b, {a, x}, []]), [b, []]),
    ?ASSERT_MATCH(lists:keydelete(a, 1, [b, {a, x}, {a, x}, {a, x}, []]), [b, {a, x}, {a, x}, []]),
    ok.

test_list_match() ->
    ?ASSERT_MATCH([], []),
    ?ASSERT_MATCH([a], [a]),
    ?ASSERT_MATCH([a,b], [a,b]),
    ?ASSERT_MATCH(id([]), []),
    ?ASSERT_MATCH(id([a]), [a]),
    ?ASSERT_MATCH(id([a,b]), [a,b]),
    ?ASSERT_MATCH(?MODULE:id([]), []),
    ?ASSERT_MATCH(?MODULE:id([a]), [a]),
    ?ASSERT_MATCH(?MODULE:id([a,b]), [a,b]),
    ok.

id(X) -> X.
