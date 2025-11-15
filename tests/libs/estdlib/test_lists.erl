%
% This file is part of AtomVM.
%
% Copyright 2019-2021 Fred Dushin <fred@dushin.net>
% Copyright 2025 migmatore <kazakvova201@gmail.com>
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

-module(test_lists).

-export([test/0, id/1]).

-include("etest.hrl").

test() ->
    ok = test_nth(),
    ok = test_nthtail(),
    ok = test_member(),
    ok = test_delete(),
    ok = test_keyfind(),
    ok = test_keydelete(),
    ok = test_keyreplace(),
    ok = test_keysort(),
    ok = test_keystore(),
    ok = test_keytake(),
    ok = test_foldl(),
    ok = test_foldr(),
    ok = test_all(),
    ok = test_any(),
    ok = test_list_match(),
    ok = test_flatten(),
    ok = test_flatmap(),
    ok = test_filter(),
    ok = test_join(),
    ok = test_seq(),
    ok = test_sort(),
    ok = test_split(),
    ok = test_usort(),
    ok = test_ukeysort(),
    ok = test_dropwhile(),
    ok = test_duplicate(),
    ok = test_filtermap(),
    ok = test_last(),
    ok = test_mapfoldl(),
    ok = test_append(),
    ok.

test_nth() ->
    ?ASSERT_MATCH(lists:nth(1, [a, b, c]), a),
    ?ASSERT_MATCH(lists:nth(2, [a, b, c]), b),
    ?ASSERT_MATCH(lists:nth(3, [a, b, c]), c),
    ?ASSERT_ERROR(lists:nth(-1, [a, b, c]), function_clause),
    ok.

test_nthtail() ->
    ?ASSERT_MATCH(lists:nthtail(0, [a, b, c]), [a, b, c]),
    ?ASSERT_MATCH(lists:nthtail(1, [a, b, c]), [b, c]),
    ?ASSERT_MATCH(lists:nthtail(2, [a, b, c]), [c]),
    ?ASSERT_MATCH(lists:nthtail(3, [a, b, c]), []),
    ?ASSERT_ERROR(lists:nthtail(-1, [a, b, c]), function_clause),
    ?ASSERT_ERROR(lists:nthtail(4, [a, b, c]), function_clause),
    ok.

test_member() ->
    ?ASSERT_TRUE(lists:member(a, [a, b, c])),
    ?ASSERT_TRUE(lists:member(b, [a, b, c])),
    ?ASSERT_TRUE(lists:member(c, [a, b, c])),
    ?ASSERT_TRUE(not lists:member(d, [a, b, c])),
    ?ASSERT_TRUE(not lists:member(x, [])),
    ok.

test_delete() ->
    ?ASSERT_MATCH(lists:delete(a, []), []),
    ?ASSERT_MATCH(lists:delete(a, [a]), []),
    ?ASSERT_MATCH(lists:delete(a, [a, b, c]), [b, c]),
    ?ASSERT_MATCH(lists:delete(a, [a, a, b, c]), [a, b, c]),
    ?ASSERT_MATCH(lists:delete(d, [a, b, c]), [a, b, c]),
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
    ?ASSERT_MATCH(
        lists:keydelete(a, 1, [b, {a, x}, {a, x}, {a, x}, []]),
        [b, {a, x}, {a, x}, []]
    ),
    ok.

test_keyreplace() ->
    ?ASSERT_MATCH(lists:keyreplace(a, 1, [], {foo, bar}), []),
    ?ASSERT_MATCH(lists:keyreplace(a, 1, [{a, x}, b, []], {1, 2}), [{1, 2}, b, []]),
    ?ASSERT_MATCH(lists:keyreplace(x, 2, [b, {a, x}, []], {1, 2}), [b, {1, 2}, []]),
    ?ASSERT_MATCH(
        lists:keyreplace(a, 1, [b, {a, x}, {a, x}, {a, x}, []], {1, 2}),
        [b, {1, 2}, {a, x}, {a, x}, []]
    ),
    ?ASSERT_MATCH(
        lists:keyreplace(a, 3, [b, {a, x}, {a, x}, {a, x}, []], {1, 2}),
        [b, {a, x}, {a, x}, {a, x}, []]
    ),
    ok.

test_keysort() ->
    ?ASSERT_MATCH(lists:keysort(1, [{2, foo}, {1, bar}]), [{1, bar}, {2, foo}]),
    ?ASSERT_MATCH(lists:keysort(1, [{2, foobar}, {1, foo}, {1, bar}]), [
        {1, foo}, {1, bar}, {2, foobar}
    ]),
    ?ASSERT_MATCH(lists:keysort(1, [{2, foo, zot}, {1, bar}]), [{1, bar}, {2, foo, zot}]),
    ?ASSERT_MATCH(lists:keysort(1, []), []),
    ?ASSERT_MATCH(lists:keysort(2, [{foo, 2}, {bar, 1}]), [{bar, 1}, {foo, 2}]),
    ?ASSERT_ERROR(lists:keysort(1, [1, 2])),
    ?ASSERT_ERROR(lists:keysort(3, [{1, bar}, {2, foo}])),

    % Our sort is always stable, but older versions of OTP only have
    % keysort/2 documented as stable
    ?ASSERT_MATCH(
        lists:keysort(1, [
            {3, a}, {2, c}, {1, z}, {2, b}, {2, a}
        ]),
        [{1, z}, {2, c}, {2, b}, {2, a}, {3, a}]
    ),
    ?ASSERT_MATCH(
        lists:keysort(1, [
            {3, a}, {1, z}, {2, c}, {2, b}, {2, a}
        ]),
        [{1, z}, {2, c}, {2, b}, {2, a}, {3, a}]
    ),
    ?ASSERT_MATCH(
        lists:keysort(1, [
            {3, a}, {2, c}, {2, b}, {1, z}, {2, a}
        ]),
        [{1, z}, {2, c}, {2, b}, {2, a}, {3, a}]
    ),
    ?ASSERT_MATCH(
        lists:keysort(1, [
            {3, a}, {2, c}, {2, b}, {2, a}, {1, z}
        ]),
        [{1, z}, {2, c}, {2, b}, {2, a}, {3, a}]
    ),

    ok.

test_keystore() ->
    ?ASSERT_MATCH(lists:keystore(a, 1, [], {foo, bar}), [{foo, bar}]),
    ?ASSERT_MATCH(lists:keystore(a, 1, [{a, x}, b, []], {1, 2}), [{1, 2}, b, []]),
    ?ASSERT_MATCH(lists:keystore(x, 2, [b, {a, x}, []], {1, 2}), [b, {1, 2}, []]),
    ?ASSERT_MATCH(
        lists:keystore(a, 1, [b, {a, x}, {a, x}, {a, x}, []], {1, 2}),
        [b, {1, 2}, {a, x}, {a, x}, []]
    ),
    ?ASSERT_MATCH(
        lists:keystore(a, 3, [b, {a, x}, {a, x}, {a, x}, []], {1, 2}),
        [b, {a, x}, {a, x}, {a, x}, [], {1, 2}]
    ),
    ok.

test_keytake() ->
    List1 = [{name, "Joe"}, {name, "Robert"}, {name, "Mike"}],
    ?ASSERT_MATCH(
        lists:keytake("Joe", 2, List1),
        {value, {name, "Joe"}, [{name, "Robert"}, {name, "Mike"}]}
    ),
    ?ASSERT_MATCH(
        lists:keytake("Robert", 2, List1),
        {value, {name, "Robert"}, [{name, "Joe"}, {name, "Mike"}]}
    ),
    ?ASSERT_MATCH(lists:keytake("Joe", 1, List1), false),
    ?ASSERT_MATCH(lists:keytake("Joe", 3, List1), false),
    ok.

test_foldl() ->
    ?ASSERT_MATCH(lists:foldl(fun(I, Accum) -> Accum + I end, 0, [1, 2, 3, 4, 5]), 15),
    ?ASSERT_MATCH(lists:foldl(fun(I, Accum) -> Accum + I end, 0, []), 0),
    ok.

test_foldr() ->
    ?ASSERT_MATCH(lists:foldr(fun(I, Accum) -> Accum + I end, 0, [1, 2, 3, 4, 5]), 15),
    ?ASSERT_MATCH(lists:foldr(fun(I, Accum) -> Accum + I end, 0, []), 0),
    ok.

test_all() ->
    ?ASSERT_MATCH(lists:all(fun(_E) -> false end, []), true),
    ?ASSERT_MATCH(lists:all(fun(_E) -> false end, [a]), false),
    ?ASSERT_MATCH(lists:all(fun(_E) -> true end, [a]), true),
    ?ASSERT_MATCH(lists:all(fun(E) -> is_atom(E) end, [a, b, c, d]), true),
    ?ASSERT_MATCH(lists:all(fun(E) -> is_atom(E) end, [a, []]), false),
    ?ASSERT_MATCH(lists:all(fun(E) -> is_atom(E) end, [[], a]), false),
    ok.

test_any() ->
    ?ASSERT_MATCH(lists:any(fun(_E) -> false end, []), false),
    ?ASSERT_MATCH(lists:any(fun(_E) -> false end, [a]), false),
    ?ASSERT_MATCH(lists:any(fun(_E) -> true end, []), false),
    ?ASSERT_MATCH(lists:any(fun(_E) -> true end, [a]), true),
    ?ASSERT_MATCH(lists:any(fun(E) -> is_atom(E) end, [a, b, c, d]), true),
    ?ASSERT_MATCH(lists:any(fun(E) -> is_atom(E) end, [a, []]), true),
    ?ASSERT_MATCH(lists:any(fun(E) -> is_atom(E) end, [[], a]), true),
    ?ASSERT_MATCH(lists:any(fun(E) -> is_atom(E) end, [[], {a}]), false),
    ok.

test_list_match() ->
    ?ASSERT_MATCH([], []),
    ?ASSERT_MATCH([a], [a]),
    ?ASSERT_MATCH([a, b], [a, b]),
    ?ASSERT_MATCH(id([]), []),
    ?ASSERT_MATCH(id([a]), [a]),
    ?ASSERT_MATCH(id([a, b]), [a, b]),
    ?ASSERT_MATCH(?MODULE:id([]), []),
    ?ASSERT_MATCH(?MODULE:id([a]), [a]),
    ?ASSERT_MATCH(?MODULE:id([a, b]), [a, b]),
    ok.

test_flatten() ->
    ?ASSERT_MATCH(lists:flatten([]), []),
    ?ASSERT_MATCH(lists:flatten([[]]), []),
    ?ASSERT_MATCH(lists:flatten([a]), [a]),
    ?ASSERT_MATCH(lists:flatten([a, []]), [a]),
    ?ASSERT_MATCH(lists:flatten([[[[[[[[a]]]]]]]]), [a]),
    ?ASSERT_MATCH(lists:flatten([a, [b]]), [a, b]),
    ?ASSERT_MATCH(lists:flatten([[a], b]), [a, b]),
    ?ASSERT_MATCH(lists:flatten([[a], [b]]), [a, b]),
    ?ASSERT_MATCH(lists:flatten([[a], [b, [c]]]), [a, b, c]),
    ?ASSERT_MATCH(lists:flatten([[a], [b, {c, [d, [e, [f]]]}]]), [a, b, {c, [d, [e, [f]]]}]),
    ?ASSERT_MATCH(
        lists:flatten([[a, b, c], [d, e, f], [g, h, i]]),
        [a, b, c, d, e, f, g, h, i]
    ),
    ?ASSERT_ERROR(lists:flatten([7 | {}])),
    ?ASSERT_ERROR(lists:flatten([[] | [5 | 5]])),
    ?ASSERT_ERROR(lists:flatten([[7 | 4], 2])),
    ok.

test_flatmap() ->
    ?ASSERT_MATCH(lists:flatmap(fun(X) -> X + 1 end, []), []),
    ?ASSERT_MATCH(lists:flatmap(fun(X) -> [X + 1] end, [1]), [2]),
    ?ASSERT_MATCH(lists:flatmap(fun(X) -> [X * X] end, [1, 2, 3, 4, 5]), [1, 4, 9, 16, 25]),
    ?ASSERT_MATCH(
        lists:flatmap(
            fun(X) ->
                case X rem 2 of
                    0 -> [X + 1];
                    1 -> [X + 2]
                end
            end,
            [1, 2]
        ),
        [3, 3]
    ),
    ?ASSERT_MATCH(
        lists:flatmap(
            fun(X) ->
                case X rem 2 of
                    0 -> [[X + 1]];
                    1 -> [X + 2]
                end
            end,
            [1, 2]
        ),
        [3, [3]]
    ),
    ?ASSERT_MATCH(
        lists:flatmap(
            fun(X) ->
                case X rem 2 of
                    0 -> [X + 1];
                    1 -> [X + 1, X + 2]
                end
            end,
            [1, 4]
        ),
        [2, 3, 5]
    ),
    ?ASSERT_ERROR(lists:flatmap(fun(X) -> X + 1 end, [1]), badarg),
    ok.

test_filter() ->
    ?ASSERT_MATCH(lists:filter(fun(I) -> I rem 2 =:= 0 end, []), []),
    ?ASSERT_MATCH(lists:filter(fun(I) -> I rem 2 =:= 0 end, [1, 2, 3, 4, 5]), [2, 4]),
    ?ASSERT_MATCH(lists:filter(fun(_) -> true end, [1, 2, 3, 4, 5]), [1, 2, 3, 4, 5]),
    ?ASSERT_MATCH(lists:filter(fun(_) -> false end, [1, 2, 3, 4, 5]), []),
    ok.

test_join() ->
    ?ASSERT_MATCH(lists:join(",", []), []),
    ?ASSERT_MATCH(lists:join(",", ["foo"]), ["foo"]),
    ?ASSERT_MATCH(lists:join(",", ["foo", "bar"]), ["foo", ",", "bar"]),
    ?ASSERT_MATCH(lists:join("", ["foo", "bar"]), ["foo", [], "bar"]),
    ok.

test_seq() ->
    ?ASSERT_MATCH(lists:seq(1, 5), [1, 2, 3, 4, 5]),
    ?ASSERT_MATCH(lists:seq(1, 5, 2), [1, 3, 5]),
    ?ASSERT_MATCH(lists:seq(1, 5, 3), [1, 4]),
    ?ASSERT_MATCH(lists:seq(1, 2, 3), [1]),
    ?ASSERT_MATCH(lists:seq(-1, 2, 3), [-1, 2]),
    ?ASSERT_MATCH(lists:seq(5, 1, -1), [5, 4, 3, 2, 1]),
    ?ASSERT_MATCH(lists:seq(1, 1, 0), [1]),
    ?ASSERT_MATCH(lists:seq(1, 1), [1]),
    ?ASSERT_MATCH(lists:seq(1, 0), []),
    ?ASSERT_MATCH(lists:seq(1, 0, 1), []),

    ?ASSERT_ERROR(lists:seq(foo, 1)),
    ?ASSERT_ERROR(lists:seq(1, bar)),
    ?ASSERT_ERROR(lists:seq(foo, bar)),
    ?ASSERT_ERROR(lists:seq(1, 2, tapas)),
    ?ASSERT_ERROR(lists:seq(1, -1)),
    ?ASSERT_ERROR(lists:seq(-1, 1, -1)),
    ?ASSERT_ERROR(lists:seq(1, -1, 1)),
    ?ASSERT_ERROR(lists:seq(1, 2, 0)),
    ok.

test_sort() ->
    ?ASSERT_MATCH(lists:sort([]), []),
    ?ASSERT_MATCH(lists:sort([1]), [1]),
    ?ASSERT_MATCH(lists:sort([1, 3, 5, 2, 4]), [1, 2, 3, 4, 5]),
    ?ASSERT_MATCH(lists:sort([1, 3, 5, 2, 3, 4]), [1, 2, 3, 3, 4, 5]),
    ?ASSERT_MATCH(lists:sort([c, a, b, d]), [a, b, c, d]),
    ?ASSERT_MATCH(lists:sort([#{}, z]), [z, #{}]),

    ?ASSERT_MATCH(lists:sort(fun(A, B) -> A > B end, [1, 2, 3, 4, 3, 5]), [5, 4, 3, 3, 2, 1]),

    ?ASSERT_ERROR(lists:sort(1), function_clause),
    ?ASSERT_ERROR(lists:sort(fun(A, B) -> A > B end, 1), function_clause),
    ?ASSERT_ERROR(lists:sort(1, [1]), function_clause),

    ok.

test_split() ->
    ?ASSERT_MATCH(
        lists:split(1, ["Foo", "Bar", "Alice", "Bob", "lowercase"]),
        {["Foo"], ["Bar", "Alice", "Bob", "lowercase"]}
    ),
    ?ASSERT_MATCH(
        lists:split(4, ["Foo", "Bar", "Alice", "Bob", "lowercase"]),
        {["Foo", "Bar", "Alice", "Bob"], ["lowercase"]}
    ),
    ?ASSERT_MATCH(lists:split(0, []), {[], []}),
    ?ASSERT_MATCH(lists:split(0, ["Foo"]), {[], ["Foo"]}),
    ?ASSERT_MATCH(lists:split(1, ["Foo"]), {["Foo"], []}),
    ?ASSERT_ERROR(lists:split(1, []), badarg),
    ?ASSERT_ERROR(lists:split(2, ["Foo"]), badarg),
    ?ASSERT_ERROR(lists:split(-1, ["Foo"]), badarg),
    ok.

test_usort() ->
    ?ASSERT_MATCH(lists:usort([]), []),
    ?ASSERT_MATCH(lists:usort([1]), [1]),
    ?ASSERT_MATCH(lists:usort([1, 3, 5, 2, 3, 4]), [1, 2, 3, 4, 5]),
    ?ASSERT_MATCH(lists:usort([1, 3, 5, 2, 1, 4]), [1, 2, 3, 4, 5]),
    ?ASSERT_MATCH(lists:usort([1, 3, 5, 2, 5, 4]), [1, 2, 3, 4, 5]),

    ?ASSERT_MATCH(
        lists:usort(fun(A, B) -> A > B end, [1, 2, 3, 4, 3, 5]),
        [5, 4, 3, 3, 2, 1]
    ),
    ?ASSERT_MATCH(lists:usort(fun(A, B) -> A >= B end, [1, 2, 3, 4, 3, 5]), [5, 4, 3, 2, 1]),

    ?ASSERT_ERROR(lists:usort(1), function_clause),
    ?ASSERT_ERROR(lists:usort(fun(A, B) -> A > B end, 1), function_clause),
    ?ASSERT_ERROR(lists:usort(1, [1]), function_clause),
    ok.

test_ukeysort() ->
    % Empty list
    ?ASSERT_MATCH(lists:ukeysort(1, []), []),

    % Single element
    ?ASSERT_MATCH(lists:ukeysort(1, [{1, a}]), [{1, a}]),

    % No duplicates
    ?ASSERT_MATCH(lists:ukeysort(1, [{2, foo}, {1, bar}]), [{1, bar}, {2, foo}]),

    % With duplicates - first occurrence kept (stable sort)
    ?ASSERT_MATCH(lists:ukeysort(1, [{2, foo}, {1, bar}, {2, baz}]), [{1, bar}, {2, foo}]),
    ?ASSERT_MATCH(lists:ukeysort(1, [{1, first}, {1, second}, {1, third}]), [{1, first}]),

    % Mixed duplicates
    ?ASSERT_MATCH(
        lists:ukeysort(1, [{3, a}, {2, b}, {1, c}, {2, d}, {3, e}]),
        [{1, c}, {2, b}, {3, a}]
    ),

    % Sorting on second element
    ?ASSERT_MATCH(lists:ukeysort(2, [{foo, 2}, {bar, 1}]), [{bar, 1}, {foo, 2}]),
    ?ASSERT_MATCH(
        lists:ukeysort(2, [{a, 2}, {b, 1}, {c, 2}]),
        [{b, 1}, {a, 2}]
    ),

    % Already sorted
    ?ASSERT_MATCH(lists:ukeysort(1, [{1, a}, {2, b}, {3, c}]), [{1, a}, {2, b}, {3, c}]),

    % Reverse sorted
    ?ASSERT_MATCH(lists:ukeysort(1, [{3, a}, {2, b}, {1, c}]), [{1, c}, {2, b}, {3, a}]),

    % Stability test - when keys are equal, first element is kept
    ?ASSERT_MATCH(
        lists:ukeysort(1, [{2, first}, {1, x}, {2, second}, {2, third}]),
        [{1, x}, {2, first}]
    ),

    ok.

test_dropwhile() ->
    ?ASSERT_MATCH(lists:dropwhile(fun(_X) -> true end, []), []),
    ?ASSERT_MATCH(lists:dropwhile(fun(_X) -> false end, []), []),
    ?ASSERT_MATCH(lists:dropwhile(fun(_X) -> false end, [1]), [1]),
    ?ASSERT_MATCH(lists:dropwhile(fun(X) -> X == 1 end, [1, 1, 1, 2, 3]), [2, 3]),
    ?ASSERT_ERROR(lists:dropwhile([], []), function_clause),
    ?ASSERT_ERROR(lists:dropwhile(fun(X) -> X == 1 end, [1 | 1]), function_clause),
    ok.

test_duplicate() ->
    ?ASSERT_MATCH(lists:duplicate(0, x), []),
    ?ASSERT_MATCH(lists:duplicate(1, x), [x]),
    ?ASSERT_MATCH(lists:duplicate(3, []), [[], [], []]),
    ?ASSERT_ERROR(lists:duplicate(-1, x), function_clause),
    ?ASSERT_ERROR(lists:duplicate(x, x), function_clause),
    ok.

test_filtermap() ->
    ?ASSERT_MATCH(
        lists:filtermap(
            fun(X) ->
                case X rem 2 of
                    0 -> {true, X div 2};
                    _ -> false
                end
            end,
            [1, 2, 3, 4, 5]
        ),
        [1, 2]
    ),
    ok.

test_last() ->
    ?ASSERT_ERROR(lists:last([]), function_clause),
    ?ASSERT_MATCH(a, lists:last([a])),
    ?ASSERT_MATCH(b, lists:last([a, b])),
    ?ASSERT_ERROR(lists:last([a | b]), function_clause),
    ok.

test_mapfoldl() ->
    ?ASSERT_MATCH({[], 1}, lists:mapfoldl(fun(X, A) -> {X * A, A + 1} end, 1, [])),
    ?ASSERT_MATCH(
        {[1, 4, 9], 4},
        lists:mapfoldl(fun(X, A) -> {X * A, A + 1} end, 1, [1, 2, 3])
    ),
    ?ASSERT_ERROR(lists:mapfoldl(fun(X, A) -> {X * A, A + 1} end, 1, foo), function_clause),
    ok.

test_append() ->
    ?ASSERT_MATCH(lists:append([]), []),
    ?ASSERT_MATCH(lists:append([[1, 2, 3, 4]]), [1, 2, 3, 4]),
    ?ASSERT_MATCH(lists:append([[1, 2], [3, 4]]), [1, 2, 3, 4]),
    ?ASSERT_MATCH(lists:append([[1, 2], [a, b]]), [1, 2, a, b]),
    ?ASSERT_MATCH(lists:append([["1", "2"], [a, b]]), ["1", "2", a, b]),
    ?ASSERT_ERROR(lists:append(1), function_clause),
    ?ASSERT_MATCH(lists:append("abc", "def"), "abcdef"),
    ?ASSERT_MATCH(lists:append([1, 2], [3, 4]), [1, 2, 3, 4]),
    ?ASSERT_ERROR(lists:append(1, 3), badarg),
    ok.

id(X) ->
    X.
