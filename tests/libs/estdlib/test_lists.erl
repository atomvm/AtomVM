-module(test_lists).

-export([test/0, id/1]).

-include("etest.hrl").
-include("estdlib.hrl").

test() ->
    ok = test_nth(),
    ok = test_member(),
    ok = test_reverse(),
    ok = test_delete(),
    ok = test_keyfind(),
    ok = test_keydelete(),
    ok = test_foldl(),
    ok = test_foldr(),
    ok = test_all(),
    ok = test_any(),
    ok = test_list_match(),
    ok = test_flatten(),
    ok.

test_nth() ->
    ?ASSERT_MATCH(?LISTS:nth(1, [a,b,c]), a),
    ?ASSERT_MATCH(?LISTS:nth(2, [a,b,c]), b),
    ?ASSERT_MATCH(?LISTS:nth(3, [a,b,c]), c),
    % try
    %     ?LISTS:nth(-1, [a,b,c]),
    %     throw(failure)
    % catch
    %     _:_ -> ok
    % end,
    ok.

test_reverse() ->
    ?ASSERT_MATCH(?LISTS:reverse([]), []),
    ?ASSERT_MATCH(?LISTS:reverse([a]), [a]),
    ?ASSERT_MATCH(?LISTS:reverse([a, b]), [b,a]),
    ok.

test_member() ->
    ?ASSERT_TRUE(?LISTS:member(a, [a,b,c])),
    ?ASSERT_TRUE(?LISTS:member(b, [a,b,c])),
    ?ASSERT_TRUE(?LISTS:member(c, [a,b,c])),
    ?ASSERT_TRUE(not ?LISTS:member(d, [a,b,c])),
    ?ASSERT_TRUE(not ?LISTS:member(x, [])),
    ok.

test_delete() ->
    ?ASSERT_MATCH(?LISTS:delete(a, []), []),
    ?ASSERT_MATCH(?LISTS:delete(a, [a]), []),
    ?ASSERT_MATCH(?LISTS:delete(a, [a,b,c]), [b,c]),
    ?ASSERT_MATCH(?LISTS:delete(a, [a,a,b,c]), [a,b,c]),
    ?ASSERT_MATCH(?LISTS:delete(d, [a,b,c]), [a,b,c]),
    ok.

test_keyfind() ->
    ?ASSERT_MATCH(?LISTS:keyfind(a, 1, [{a, x}, b, []]), {a, x}),
    ?ASSERT_MATCH(?LISTS:keyfind(x, 2, [{a, x}, b, []]), {a, x}),
    ?ASSERT_MATCH(?LISTS:keyfind(x, 3, [{a, x}, b, []]), false),
    ?ASSERT_MATCH(?LISTS:keyfind(b, 1, [{a, 1}, b, []]), false),

    ?ASSERT_MATCH(?LISTS:keyfind(b, 1, [{a, x}, {b, foo, bar}, []]), {b, foo, bar}),
    ?ASSERT_MATCH(?LISTS:keyfind(nope, 2, [{a, x}, {b, foo, bar}, []]), false),
    ok.

test_keydelete() ->
    ?ASSERT_MATCH(?LISTS:keydelete(a, 1, []), []),
    ?ASSERT_MATCH(?LISTS:keydelete(a, 1, [{a, x}, b, []]), [b, []]),
    ?ASSERT_MATCH(?LISTS:keydelete(a, 1, [b, {a, x}, []]), [b, []]),
    ?ASSERT_MATCH(?LISTS:keydelete(a, 1, [b, {a, x}, {a, x}, {a, x}, []]), [b, {a, x}, {a, x}, []]),
    ok.

test_foldl() ->
    ?ASSERT_MATCH(?LISTS:foldl(fun(I, Accum) -> Accum + I end, 0, [1,2,3,4,5]), 15),
    ?ASSERT_MATCH(?LISTS:foldl(fun(I, Accum) -> Accum + I end, 0, []), 0),
    ok.

test_foldr() ->
    ?ASSERT_MATCH(?LISTS:foldr(fun(I, Accum) -> Accum + I end, 0, [1,2,3,4,5]), 15),
    ?ASSERT_MATCH(?LISTS:foldr(fun(I, Accum) -> Accum + I end, 0, []), 0),
    ok.

test_all() ->
    ?ASSERT_MATCH(?LISTS:all(fun(_E) -> false end, []), true),
    ?ASSERT_MATCH(?LISTS:all(fun(_E) -> false end, [a]), false),
    ?ASSERT_MATCH(?LISTS:all(fun(_E) -> true end, [a]), true),
    ?ASSERT_MATCH(?LISTS:all(fun(E) -> is_atom(E) end, [a,b,c,d]), true),
    ?ASSERT_MATCH(?LISTS:all(fun(E) -> is_atom(E) end, [a, []]), false),
    ?ASSERT_MATCH(?LISTS:all(fun(E) -> is_atom(E) end, [[], a]), false),
    ok.

test_any() ->
    ?ASSERT_MATCH(?LISTS:any(fun(_E) -> false end, []), false),
    ?ASSERT_MATCH(?LISTS:any(fun(_E) -> false end, [a]), false),
    ?ASSERT_MATCH(?LISTS:any(fun(_E) -> true end, []), false),
    ?ASSERT_MATCH(?LISTS:any(fun(_E) -> true end, [a]), true),
    ?ASSERT_MATCH(?LISTS:any(fun(E) -> is_atom(E) end, [a,b,c,d]), true),
    ?ASSERT_MATCH(?LISTS:any(fun(E) -> is_atom(E) end, [a, []]), true),
    ?ASSERT_MATCH(?LISTS:any(fun(E) -> is_atom(E) end, [[], a]), true),
    ?ASSERT_MATCH(?LISTS:any(fun(E) -> is_atom(E) end, [[], {a}]), false),
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

test_flatten() ->
    ?ASSERT_MATCH(?LISTS:flatten([]), []),
    ?ASSERT_MATCH(?LISTS:flatten([a]), [a]),
    ?ASSERT_MATCH(?LISTS:flatten([a, []]), [a]),
    ?ASSERT_MATCH(?LISTS:flatten([[[[[[[[a]]]]]]]]), [a]),
    ?ASSERT_MATCH(?LISTS:flatten([a,[b]]), [a,b]),
    ?ASSERT_MATCH(?LISTS:flatten([[a],b]), [a,b]),
    ?ASSERT_MATCH(?LISTS:flatten([[a],[b]]), [a,b]),
    ?ASSERT_MATCH(?LISTS:flatten([[a],[b, [c]]]), [a,b,c]),
    ?ASSERT_MATCH(?LISTS:flatten([[a],[b, {c, [d, [e, [f]]]}]]), [a,b,{c, [d, [e, [f]]]}]),
    ?ASSERT_MATCH(?LISTS:flatten([[a,b,c],[d,e,f],[g,h,i]]), [a,b,c,d,e,f,g,h,i]),
    ok.

id(X) -> X.
