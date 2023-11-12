%
% This file is part of AtomVM.
%
% Copyright 2019-2021 Fred Dushin <fred@dushin.net>
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
    ok = test_member(),
    ok = test_delete(),
    ok = test_keyfind(),
    ok = test_keydelete(),
    ok = test_keyreplace(),
    ok = test_foldl(),
    ok = test_foldr(),
    ok = test_all(),
    ok = test_any(),
    ok = test_list_match(),
    ok = test_flatten(),
    ok = test_filter(),
    ok = test_join(),
    ok = test_seq(),
    ok = test_sort(),
    ok = test_usort(),
    ok.

test_nth() ->
    ?ASSERT_MATCH(lists:nth(1, [a, b, c]), a),
    ?ASSERT_MATCH(lists:nth(2, [a, b, c]), b),
    ?ASSERT_MATCH(lists:nth(3, [a, b, c]), c),
    ?ASSERT_ERROR(lists:nth(-1, [a, b, c]), function_clause),
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
    ?ASSERT_MATCH(lists:keydelete(a, 1, [b, {a, x}, {a, x}, {a, x}, []]), [b, {a, x}, {a, x}, []]),
    ok.

test_keyreplace() ->
    ?ASSERT_MATCH(lists:keyreplace(a, 1, [], {foo, bar}), []),
    ?ASSERT_MATCH(lists:keyreplace(a, 1, [{a, x}, b, []], {1, 2}), [{1, 2}, b, []]),
    ?ASSERT_MATCH(lists:keyreplace(x, 2, [b, {a, x}, []], {1, 2}), [b, {1, 2}, []]),
    ?ASSERT_MATCH(lists:keyreplace(a, 1, [b, {a, x}, {a, x}, {a, x}, []], {1, 2}), [
        b,
        {1, 2},
        {a, x},
        {a, x},
        []
    ]),
    ?ASSERT_MATCH(lists:keyreplace(a, 3, [b, {a, x}, {a, x}, {a, x}, []], {1, 2}), [
        b,
        {a, x},
        {a, x},
        {a, x},
        []
    ]),
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
    ?ASSERT_MATCH(lists:flatten([a]), [a]),
    ?ASSERT_MATCH(lists:flatten([a, []]), [a]),
    ?ASSERT_MATCH(lists:flatten([[[[[[[[a]]]]]]]]), [a]),
    ?ASSERT_MATCH(lists:flatten([a, [b]]), [a, b]),
    ?ASSERT_MATCH(lists:flatten([[a], b]), [a, b]),
    ?ASSERT_MATCH(lists:flatten([[a], [b]]), [a, b]),
    ?ASSERT_MATCH(lists:flatten([[a], [b, [c]]]), [a, b, c]),
    ?ASSERT_MATCH(lists:flatten([[a], [b, {c, [d, [e, [f]]]}]]), [a, b, {c, [d, [e, [f]]]}]),
    ?ASSERT_MATCH(lists:flatten([[a, b, c], [d, e, f], [g, h, i]]), [a, b, c, d, e, f, g, h, i]),
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

test_usort() ->
    ?ASSERT_MATCH(lists:usort([]), []),
    ?ASSERT_MATCH(lists:usort([1]), [1]),
    ?ASSERT_MATCH(lists:usort([1, 3, 5, 2, 3, 4]), [1, 2, 3, 4, 5]),
    ?ASSERT_MATCH(lists:usort([1, 3, 5, 2, 1, 4]), [1, 2, 3, 4, 5]),
    ?ASSERT_MATCH(lists:usort([1, 3, 5, 2, 5, 4]), [1, 2, 3, 4, 5]),

    ?ASSERT_MATCH(lists:usort(fun(A, B) -> A > B end, [1, 2, 3, 4, 3, 5]), [5, 4, 3, 3, 2, 1]),
    ?ASSERT_MATCH(lists:usort(fun(A, B) -> A >= B end, [1, 2, 3, 4, 3, 5]), [5, 4, 3, 2, 1]),

    ?ASSERT_ERROR(lists:usort(1), function_clause),
    ?ASSERT_ERROR(lists:usort(fun(A, B) -> A > B end, 1), function_clause),
    ?ASSERT_ERROR(lists:usort(1, [1]), function_clause),

    ok.

id(X) -> X.
