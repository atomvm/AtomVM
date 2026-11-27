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
    ok = test_keyed_validation(),
    ok = test_keysort_inputs(),
    ok = test_foldl(),
    ok = test_foldr(),
    ok = test_fold_validation(),
    ok = test_fold_exception_boundaries(),
    ok = test_unary_validation(),
    ok = test_unary_exception_boundaries(),
    ok = test_search_results(),
    ok = test_all(),
    ok = test_any(),
    ok = test_predicate_results(),
    ok = test_list_match(),
    ok = test_flatten(),
    ok = test_flatmap(),
    ok = test_filter(),
    ok = test_join(),
    ok = test_seq(),
    ok = test_sort(),
    ok = test_merge(),
    ok = test_ordering_validation(),
    ok = test_split(),
    ok = test_usort(),
    ok = test_ukeysort(),
    ok = test_dropwhile(),
    ok = test_duplicate(),
    ok = test_filtermap(),
    ok = test_last(),
    ok = test_mapfoldl(),
    ok = test_append(),
    ok = test_min(),
    ok = test_max(),
    ok = test_sublist(),
    ok = test_sublist3(),
    ok = test_droplast(),
    ok = test_zip(),
    ok = test_unzip(),
    ok = test_zipwith(),
    ok = test_sum(),
    ok.

test_nth() ->
    ?ASSERT_MATCH(lists:nth(1, [a, b, c]), a),
    ?ASSERT_MATCH(lists:nth(2, [a, b, c]), b),
    ?ASSERT_MATCH(lists:nth(3, [a, b, c]), c),
    ?ASSERT_ERROR(lists:nth(-1, [a, b, c]), function_clause),
    ?ASSERT_ERROR(lists:nth(bad, [a]), function_clause),
    ?ASSERT_ERROR(lists:nth({bad, index}, [a, b]), function_clause),
    ?ASSERT_ERROR(lists:nth(2.0, [a, b]), function_clause),
    ?ASSERT_MATCH(lists:nth(2, [a, b | tail]), b),
    ok.

test_nthtail() ->
    ?ASSERT_MATCH(lists:nthtail(0, [a, b, c]), [a, b, c]),
    ?ASSERT_MATCH(lists:nthtail(1, [a, b, c]), [b, c]),
    ?ASSERT_MATCH(lists:nthtail(2, [a, b, c]), [c]),
    ?ASSERT_MATCH(lists:nthtail(3, [a, b, c]), []),
    ?ASSERT_ERROR(lists:nthtail(-1, [a, b, c]), function_clause),
    ?ASSERT_ERROR(lists:nthtail(4, [a, b, c]), function_clause),
    ?ASSERT_MATCH(lists:nthtail(1, [a | tail]), tail),
    ?ASSERT_MATCH(lists:nthtail(2, [a, b | tail]), tail),
    ?ASSERT_MATCH(lists:nthtail(1, [a, b | tail]), [b | tail]),
    ?ASSERT_ERROR(lists:nthtail(3, [a, b | tail]), function_clause),
    ?ASSERT_ERROR(lists:nthtail(0, tail), function_clause),
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
    ?ASSERT_MATCH(
        lists:keydelete(1, 1, [skip, {}, {1.0, first}, {1, later} | tail]),
        [skip, {}, {1, later} | tail]
    ),
    ?ASSERT_MATCH(lists:keydelete(1, 2, [{only, 1.0}]), []),
    ?ASSERT_MATCH(lists:keydelete({1}, 1, [{{1.0}, first}, {{1}, later}]), [{{1}, later}]),
    ?ASSERT_MATCH(
        lists:keydelete(9007199254740993, 1, [
            {9007199254740992.0, keep}, {9007199254740993, remove}
        ]),
        [{9007199254740992.0, keep}]
    ),
    ?ASSERT_ERROR(lists:keydelete(missing, 1, [{1, first} | tail]), function_clause),
    ok.

test_keyed_validation() ->
    lists:foreach(
        fun(Position) ->
            lists:foreach(
                fun(Items) ->
                    ?ASSERT_ERROR(lists:keydelete(a, Position, Items), function_clause),
                    ?ASSERT_ERROR(lists:keyreplace(a, Position, Items, {}), function_clause),
                    ?ASSERT_ERROR(lists:keystore(a, Position, Items, {}), function_clause),
                    ?ASSERT_ERROR(lists:keytake(a, Position, Items), function_clause),
                    ?ASSERT_ERROR(lists:keysort(Position, Items), function_clause),
                    ?ASSERT_ERROR(lists:ukeysort(Position, Items), function_clause)
                end,
                [[], [{a, first}]]
            )
        end,
        [id(0), id(-1), id(1.0), id(bad_position)]
    ),
    lists:foreach(
        fun(Replacement) ->
            ?ASSERT_ERROR(lists:keyreplace(a, 1, [], Replacement), function_clause),
            ?ASSERT_ERROR(lists:keystore(a, 1, [], Replacement), function_clause)
        end,
        [id(not_tuple), id([])]
    ),
    ok.

test_keysort_inputs() ->
    lists:foreach(
        fun(Operation) ->
            ?ASSERT_MATCH(apply(lists, Operation, [3, []]), []),
            ?ASSERT_MATCH(apply(lists, Operation, [3, [not_tuple]]), [not_tuple]),
            ?ASSERT_MATCH(apply(lists, Operation, [3, [{}]]), [{}]),
            ?ASSERT_ERROR(apply(lists, Operation, [3, [{a}, {b}]]), badarg),
            lists:foreach(
                fun(Input) ->
                    ?ASSERT_ERROR(
                        apply(lists, Operation, [1, Input]), {case_clause, Input}
                    )
                end,
                [id(not_list), id({not_a, list}), id([{a, first} | tail])]
            ),
            ?ASSERT_ERROR(
                apply(lists, Operation, [1, [{a, first}, {b, second} | tail]]),
                function_clause
            )
        end,
        [keysort, ukeysort]
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
    ?ASSERT_MATCH(
        lists:keyreplace(1, 1, [skip, {}, {1.0, first}, {1, later} | tail], {changed, value}),
        [skip, {}, {changed, value}, {1, later} | tail]
    ),
    ?ASSERT_MATCH(lists:keyreplace(1, 2, [{only, 1.0}], {}), [{}]),
    ?ASSERT_MATCH(
        lists:keyreplace({1}, 2, [{first, {1.0}}, {later, {1}}], {changed, value}),
        [{changed, value}, {later, {1}}]
    ),
    ?ASSERT_ERROR(lists:keyreplace(missing, 1, [{1, first} | tail], {}), function_clause),
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
    ?ASSERT_MATCH(
        lists:keystore(1, 1, [skip, {}, {1.0, first}, {1, later} | tail], {changed, value}),
        [skip, {}, {changed, value}, {1, later} | tail]
    ),
    ?ASSERT_MATCH(lists:keystore(1, 2, [{only, 1.0}], {}), [{}]),
    ?ASSERT_MATCH(
        lists:keystore({1}, 2, [{first, {1.0}}, {later, {1}}], {changed, value}),
        [{changed, value}, {later, {1}}]
    ),
    ?ASSERT_ERROR(lists:keystore(missing, 1, [{1, first} | tail], {}), function_clause),
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

test_fold_validation() ->
    lists:foreach(
        fun(Operation) ->
            lists:foreach(
                fun(Callback) ->
                    lists:foreach(
                        fun(Input) ->
                            ?ASSERT_ERROR(
                                apply(lists, Operation, [Callback, seed, Input]), function_clause
                            )
                        end,
                        [[], [1.0], [3, 7 | tail], not_a_list]
                    )
                end,
                [not_fun, fun(_) -> error(unexpected_callback) end]
            )
        end,
        [foldl, foldr, mapfoldl, mapfoldr]
    ),
    ok.

test_fold_exception_boundaries() ->
    Seed = {seed, <<5:3>>},
    LeftTrace = [{3, Seed}, {7, {3, Seed}}],
    RightTrace = [{2, Seed}, {7, {2, Seed}}],
    lists:foreach(
        fun(Operation) ->
            {Prefix, ImproperTrace} =
                case Operation of
                    foldl -> {LeftTrace, LeftTrace};
                    mapfoldl -> {LeftTrace, LeftTrace};
                    _ -> {RightTrace, []}
                end,
            EntryReason =
                case Operation of
                    foldl -> {case_clause, not_a_list};
                    _ -> function_clause
                end,
            lists:foreach(
                fun({Input, Mode, Expected, Trace}) ->
                    put(fold_calls, []),
                    Callback = fun(Item, Acc) ->
                        Calls = [{Item, Acc} | get(fold_calls)],
                        put(fold_calls, Calls),
                        true = erlang:garbage_collect(),
                        case {Mode, length(Calls)} of
                            {{fail, error, Reason}, 2} ->
                                error(Reason);
                            {{fail, throw, Reason}, 2} ->
                                throw(Reason);
                            {{fail, exit, Reason}, 2} ->
                                exit(Reason);
                            _ ->
                                Next = {Item, Acc},
                                case Operation of
                                    foldl -> Next;
                                    foldr -> Next;
                                    _ -> {Item, Next}
                                end
                        end
                    end,
                    Outcome =
                        try apply(lists, Operation, [Callback, Seed, Input]) of
                            _ -> unexpected_success
                        catch
                            Class:Reason -> {Class, Reason}
                        end,
                    ?ASSERT_MATCH(Outcome, Expected),
                    Calls = lists:reverse(erase(fold_calls)),
                    ?ASSERT_MATCH(Calls, Trace)
                end,
                [
                    {not_a_list, normal, {error, EntryReason}, []},
                    {[3, 7 | tail], normal, {error, function_clause}, ImproperTrace}
                ] ++
                    [
                        {[3, 7, 2], {fail, Class, Reason}, {Class, Reason}, Prefix}
                     || {Class, Reason} <- [
                            {error, badarg},
                            {error, function_clause},
                            {error, {case_clause, from_callback}},
                            {throw, callback_failed},
                            {exit, callback_failed}
                        ]
                    ]
            )
        end,
        [foldl, foldr, mapfoldl, mapfoldr]
    ),
    ok.

test_unary_validation() ->
    % Keep the regression driver independent of foreach/2, which is under test.
    lists:foldl(
        fun(Operation, ok) ->
            lists:foldl(
                fun(Callback, ok) ->
                    lists:foldl(
                        fun(Input, ok) ->
                            ?ASSERT_ERROR(
                                apply(lists, Operation, [Callback, Input]), function_clause
                            )
                        end,
                        ok,
                        [[], [1.0], [3, 7 | tail], not_a_list]
                    )
                end,
                ok,
                [not_fun, fun(_, _) -> error(unexpected_callback) end]
            )
        end,
        ok,
        [map, foreach, flatmap, search]
    ),
    ok.

test_unary_exception_boundaries() ->
    lists:foldl(
        fun(Operation, ok) ->
            EntryReason =
                case Operation of
                    map -> {case_clause, not_a_list};
                    _ -> function_clause
                end,
            lists:foldl(
                fun({Input, Mode, Expected, Trace}, ok) ->
                    put(unary_calls, []),
                    Callback = fun(Item) ->
                        put(unary_calls, [Item | get(unary_calls)]),
                        true = erlang:garbage_collect(),
                        case {Mode, Item} of
                            {{fail, error, Reason}, 7} ->
                                error(Reason);
                            {{fail, throw, Reason}, 7} ->
                                throw(Reason);
                            {{fail, exit, Reason}, 7} ->
                                exit(Reason);
                            _ ->
                                case Operation of
                                    flatmap -> [Item];
                                    search -> false;
                                    _ -> {Item, <<5:3>>}
                                end
                        end
                    end,
                    Outcome =
                        try apply(lists, Operation, [Callback, Input]) of
                            _ -> unexpected_success
                        catch
                            Class:Reason -> {Class, Reason}
                        end,
                    ?ASSERT_MATCH(Outcome, Expected),
                    Calls = lists:reverse(erase(unary_calls)),
                    ?ASSERT_MATCH(Calls, Trace)
                end,
                ok,
                [
                    {not_a_list, normal, {error, EntryReason}, []},
                    {[3, 7 | tail], normal, {error, function_clause}, [3, 7]}
                ] ++
                    [
                        {[3, 7, 2], {fail, Class, Reason}, {Class, Reason}, [3, 7]}
                     || {Class, Reason} <- [
                            {error, badarg},
                            {error, function_clause},
                            {error, {case_clause, from_callback}},
                            {throw, callback_failed},
                            {exit, callback_failed}
                        ]
                    ]
            )
        end,
        ok,
        [map, foreach, flatmap, search]
    ),
    Deferred =
        try
            lists:flatmap(
                fun
                    (3) -> not_a_list;
                    (7) -> throw(later)
                end,
                [3, 7]
            )
        of
            _ -> unexpected_success
        catch
            Class:Reason -> {Class, Reason}
        end,
    ?ASSERT_MATCH(Deferred, {throw, later}),
    ok.

test_search_results() ->
    lists:foldl(
        fun(Bad, ok) ->
            put(search_calls, []),
            Callback = fun(Item) ->
                put(search_calls, [Item | get(search_calls)]),
                true = erlang:garbage_collect(),
                case Item of
                    3 -> false;
                    7 -> Bad;
                    _ -> error(unnecessary_callback)
                end
            end,
            Outcome =
                try lists:search(Callback, [3, 7, 2]) of
                    _ -> unexpected_success
                catch
                    Class:Reason -> {Class, Reason}
                end,
            ?ASSERT_MATCH(Outcome, {error, {case_clause, Bad}}),
            Calls = lists:reverse(erase(search_calls)),
            ?ASSERT_MATCH(Calls, [3, 7])
        end,
        ok,
        [not_boolean, [], {true, changed}]
    ),
    ?ASSERT_MATCH(lists:search(fun(X) -> X =:= 1 end, [1.0, 1 | tail]), {value, 1}),
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

test_predicate_results() ->
    lists:foreach(
        fun({Operation, Continue, Stop}) ->
            lists:foreach(
                fun(Invalid) ->
                    put(predicate_calls, []),
                    Predicate = fun(Item) ->
                        put(predicate_calls, [Item | get(predicate_calls)]),
                        case Item of
                            first -> Continue;
                            invalid -> Invalid;
                            later -> error(unexpected_callback)
                        end
                    end,
                    ?ASSERT_ERROR(
                        apply(lists, Operation, [Predicate, [first, invalid, later]]),
                        {case_clause, Invalid}
                    ),
                    ?ASSERT_MATCH(erase(predicate_calls), [invalid, first])
                end,
                [not_boolean, 0, {true, mapped}]
            ),
            put(predicate_calls, []),
            ShortCircuit = fun(Item) ->
                put(predicate_calls, [Item | get(predicate_calls)]),
                case Item of
                    first -> Stop;
                    later -> error(unexpected_callback)
                end
            end,
            ?ASSERT_MATCH(apply(lists, Operation, [ShortCircuit, [first, later]]), Stop),
            ?ASSERT_MATCH(erase(predicate_calls), [first])
        end,
        [{all, true, false}, {any, false, true}]
    ),
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
    ?ASSERT_MATCH(lists:seq(id(5), id(4), id(2)), []),
    ?ASSERT_MATCH(lists:seq(id(5), id(3), id(2)), []),
    ?ASSERT_MATCH(lists:seq(id(10), id(7), id(5)), []),
    ?ASSERT_MATCH(lists:seq(id(10), id(5), id(5)), []),
    ?ASSERT_MATCH(lists:seq(id(4), id(5), id(-2)), []),
    ?ASSERT_MATCH(lists:seq(id(3), id(5), id(-2)), []),
    ?ASSERT_MATCH(lists:seq(id(7), id(10), id(-5)), []),
    ?ASSERT_MATCH(lists:seq(id(5), id(10), id(-5)), []),

    ?ASSERT_ERROR(lists:seq(foo, 1)),
    ?ASSERT_ERROR(lists:seq(1, bar)),
    ?ASSERT_ERROR(lists:seq(foo, bar)),
    ?ASSERT_ERROR(lists:seq(1, 2, tapas)),
    ?ASSERT_ERROR(lists:seq(1, -1)),
    ?ASSERT_ERROR(lists:seq(-1, 1, -1)),
    ?ASSERT_ERROR(lists:seq(1, -1, 1)),
    ?ASSERT_ERROR(lists:seq(1, 2, 0)),
    ?ASSERT_ERROR(lists:seq(id(5), id(2), id(2)), badarg),
    ?ASSERT_ERROR(lists:seq(id(10), id(4), id(5)), badarg),
    ?ASSERT_ERROR(lists:seq(id(2), id(5), id(-2)), badarg),
    ?ASSERT_ERROR(lists:seq(id(4), id(10), id(-5)), badarg),
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

    Items = [{2, a}, {1, first}, {2, b}, {1.0, second}, {3, z}, {2, c}],
    ?ASSERT_MATCH(
        lists:sort(fun({A, _}, {B, _}) -> A =< B end, Items),
        [{1, first}, {1.0, second}, {2, a}, {2, b}, {2, c}, {3, z}]
    ),
    ?ASSERT_MATCH(
        lists:sort(fun({A, _}, {B, _}) -> A >= B end, Items),
        [{3, z}, {2, a}, {2, b}, {2, c}, {1, first}, {1.0, second}]
    ),
    ?ASSERT_MATCH(lists:sort([1.0, 1, 1.0, 1]), [1.0, 1, 1.0, 1]),
    ok.

test_merge() ->
    Ascending = fun({A, _}, {B, _}) -> A =< B end,
    Descending = fun({A, _}, {B, _}) -> A >= B end,
    ?ASSERT_MATCH(
        lists:merge(Ascending, [{1, a}, {1, b}, {3, c}], [{1.0, d}, {2, e}, {3, f}]),
        [{1, a}, {1, b}, {1.0, d}, {2, e}, {3, c}, {3, f}]
    ),
    ?ASSERT_MATCH(
        lists:merge(Descending, [{3, c}, {1, a}, {1, b}], [{3, f}, {2, e}, {1.0, d}]),
        [{3, c}, {3, f}, {2, e}, {1, a}, {1, b}, {1.0, d}]
    ),
    ?ASSERT_MATCH(lists:merge([1, 1.0, 3], [1.0, 2, 3.0]), [1, 1.0, 1.0, 2, 3, 3.0]),
    ok.

test_ordering_validation() ->
    lists:foreach(
        fun(Callback) ->
            ?ASSERT_ERROR(lists:sort(Callback, []), function_clause),
            ?ASSERT_ERROR(lists:sort(Callback, [3]), function_clause),
            Reason =
                case is_function(Callback) of
                    true -> {badarity, {Callback, [3, 1]}};
                    false -> {badfun, Callback}
                end,
            ?ASSERT_ERROR(lists:sort(Callback, [3, 1]), Reason),
            lists:foreach(
                fun(Items) ->
                    ?ASSERT_ERROR(lists:usort(Callback, Items), function_clause)
                end,
                [[], [3], [3, 1]]
            ),
            lists:foreach(
                fun({Left, Right}) ->
                    ?ASSERT_ERROR(lists:merge(Callback, Left, Right), function_clause)
                end,
                [{[], []}, {[3], []}, {[], [1]}, {[3], [1]}]
            )
        end,
        [id(not_fun), fun() -> wrong end, fun(_) -> wrong end, fun(_, _, _) -> wrong end]
    ),
    Unused = fun(_, _) -> error(unexpected_comparison) end,
    ?ASSERT_MATCH(lists:sort(Unused, []), []),
    ?ASSERT_MATCH(lists:sort(Unused, [3]), [3]),
    ?ASSERT_MATCH(lists:usort(Unused, []), []),
    ?ASSERT_MATCH(lists:usort(Unused, [3]), [3]),
    ?ASSERT_MATCH(lists:merge(Unused, [], []), []),
    ?ASSERT_MATCH(lists:merge(Unused, [3], []), [3]),
    ?ASSERT_MATCH(lists:merge(Unused, [], [1]), [1]),
    lists:foreach(
        fun(Operation) ->
            Arguments =
                case Operation of
                    merge -> [[3], [1]];
                    _ -> [[3, 1]]
                end,
            lists:foreach(
                fun(Class) ->
                    Callback = fun(_, _) ->
                        case Class of
                            error -> error(comparator_failed);
                            throw -> throw(comparator_failed);
                            exit -> exit(comparator_failed)
                        end
                    end,
                    ?ASSERT_EXCEPTION(
                        apply(lists, Operation, [Callback | Arguments]), Class, comparator_failed
                    )
                end,
                [error, throw, exit]
            ),
            ?ASSERT_ERROR(
                apply(lists, Operation, [fun(_, _) -> not_boolean end | Arguments]),
                {case_clause, not_boolean}
            )
        end,
        [sort, usort, merge]
    ),
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
    Items = [{2, first}, {1, a}, {2, second}, {1.0, b}, {3, z}, {2, third}],
    ?ASSERT_MATCH(
        lists:usort(fun({A, _}, {B, _}) -> A =< B end, Items),
        [{1, a}, {2, first}, {3, z}]
    ),
    ?ASSERT_MATCH(
        lists:usort(fun({A, _}, {B, _}) -> A >= B end, Items),
        [{3, z}, {2, first}, {1, a}]
    ),
    ?ASSERT_MATCH(lists:usort([1.0, 1, 1]), [1.0]),
    ?ASSERT_MATCH(lists:usort([1, 1.0, 1.0]), [1]),
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
    ?ASSERT_MATCH(
        lists:ukeysort(1, [{2, last}, {1.0, first}, {1, later}, {2.0, duplicate}]),
        [{1.0, first}, {2, last}]
    ),
    ?ASSERT_MATCH(
        lists:ukeysort(2, [{first, 1}, {later, 1.0}, {keep, 2}]),
        [{first, 1}, {keep, 2}]
    ),
    ?ASSERT_MATCH(
        lists:ukeysort(1, [
            {9007199254740993, keep}, {9007199254740992.0, first}, {9007199254740992, later}
        ]),
        [{9007199254740992.0, first}, {9007199254740993, keep}]
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
    ok = test_mapfoldr(),
    ok.

test_mapfoldr() ->
    ?ASSERT_MATCH({[], 1}, lists:mapfoldr(fun(X, A) -> {X * A, A + 1} end, 1, [])),
    %% Right-to-left: the accumulator visits 3, then 2, then 1.
    ?ASSERT_MATCH(
        {[3, 4, 3], 4},
        lists:mapfoldr(fun(X, A) -> {X * A, A + 1} end, 1, [1, 2, 3])
    ),
    ?ASSERT_ERROR(lists:mapfoldr(fun(X, A) -> {X * A, A + 1} end, 1, foo), function_clause),
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

test_min() ->
    ?ASSERT_MATCH(lists:min([1]), 1),
    ?ASSERT_MATCH(lists:min([1, 2, 3]), 1),
    ?ASSERT_MATCH(lists:min([3, 1, 2]), 1),
    ?ASSERT_MATCH(lists:min([3, 2, 1]), 1),
    ?ASSERT_MATCH(lists:min([-5, 0, 5]), -5),
    ?ASSERT_MATCH(lists:min([a, b, c]), a),
    ?ASSERT_MATCH(lists:min([c, b, a]), a),
    ?ASSERT_ERROR(lists:min([]), function_clause),
    ok.

test_max() ->
    ?ASSERT_MATCH(lists:max([1]), 1),
    ?ASSERT_MATCH(lists:max([1, 2, 3]), 3),
    ?ASSERT_MATCH(lists:max([3, 1, 2]), 3),
    ?ASSERT_MATCH(lists:max([1, 3, 2]), 3),
    ?ASSERT_MATCH(lists:max([-5, 0, 5]), 5),
    ?ASSERT_MATCH(lists:max([a, b, c]), c),
    ?ASSERT_MATCH(lists:max([c, b, a]), c),
    ?ASSERT_ERROR(lists:max([]), function_clause),
    ok.

test_sublist() ->
    ?ASSERT_MATCH(lists:sublist([], 0), []),
    ?ASSERT_MATCH(lists:sublist([a, b], 0), []),
    ?ASSERT_MATCH(lists:sublist([a, b], 3), [a, b]),
    ?ASSERT_MATCH(lists:sublist([a, b | tail], 2), [a, b]),
    ?ASSERT_ERROR(lists:sublist([a, b | tail], 3), function_clause),
    ?ASSERT_ERROR(lists:sublist(tail, 0), function_clause),
    ?ASSERT_ERROR(lists:sublist(#{a => 1}, 0), function_clause),
    ok.

test_sublist3() ->
    ?ASSERT_MATCH(lists:sublist([a, b, c, d, e], 1, 3), [a, b, c]),
    ?ASSERT_MATCH(lists:sublist([a, b, c, d, e], 2, 3), [b, c, d]),
    ?ASSERT_MATCH(lists:sublist([a, b, c, d, e], 3, 10), [c, d, e]),
    ?ASSERT_MATCH(lists:sublist([a, b, c], 1, 0), []),
    ?ASSERT_MATCH(lists:sublist([], 1, 0), []),
    ?ASSERT_ERROR(lists:sublist([a, b, c], 0, 1), function_clause),
    ?ASSERT_ERROR(lists:sublist([a, b, c], 1, -1), function_clause),
    ?ASSERT_MATCH(lists:sublist([a, b, c], 5, 1), []),
    ok.

test_droplast() ->
    ?ASSERT_MATCH(lists:droplast([a]), []),
    ?ASSERT_MATCH(lists:droplast([a, b]), [a]),
    ?ASSERT_MATCH(lists:droplast([a, b, c]), [a, b]),
    ?ASSERT_ERROR(lists:droplast([]), function_clause),
    ok.

test_zip() ->
    ?ASSERT_MATCH(lists:zip([a, b, c], [1, 2, 3]), [{a, 1}, {b, 2}, {c, 3}]),
    ?ASSERT_MATCH(lists:zip([], []), []),
    ?ASSERT_MATCH(lists:zip([a], [1]), [{a, 1}]),
    ?ASSERT_ERROR(lists:zip([a, b], [1]), function_clause),
    ?ASSERT_ERROR(lists:zip([a], [1, 2]), function_clause),
    ok.

test_unzip() ->
    ?ASSERT_MATCH(lists:unzip(id([])), {[], []}),
    ?ASSERT_MATCH(lists:unzip(id([{a, 7}])), {[a], [7]}),
    Pairs = id([{1, one}, {1.0, <<5:3>>}, {1, {nested, #{x => 7}}}, {a, [1 | tail]}]),
    ?ASSERT_MATCH(
        lists:unzip(Pairs),
        {[1, 1.0, 1, a], [one, <<5:3>>, {nested, #{x => 7}}, [1 | tail]]}
    ),
    ?ASSERT_ERROR(lists:unzip(id(bad)), function_clause),
    ?ASSERT_ERROR(lists:unzip(id([{a, 1}, bad])), function_clause),
    ?ASSERT_ERROR(lists:unzip(id([{a, 1} | tail])), function_clause),
    ?ASSERT_ERROR(lists:unzip(id([{a, 1, extra}])), function_clause),
    ok.

test_zipwith() ->
    ?ASSERT_MATCH(lists:zipwith(fun(A, B) -> {A, B} end, [a, b, c], [1, 2, 3]), [
        {a, 1}, {b, 2}, {c, 3}
    ]),
    ?ASSERT_MATCH(lists:zipwith(fun(A, B) -> A + B end, [1, 2, 3], [10, 20, 30]), [11, 22, 33]),
    ?ASSERT_MATCH(lists:zipwith(fun(_, _) -> ok end, [], []), []),
    ?ASSERT_ERROR(lists:zipwith(fun(A, B) -> {A, B} end, [a], [1, 2]), function_clause),
    ok.

test_sum() ->
    ?ASSERT_MATCH(lists:sum(id([])), 0),
    ?ASSERT_MATCH(lists:sum(id([9, -4, 2])), 7),
    Big = 1 bsl 100,
    ?ASSERT_MATCH(lists:sum(id([Big, 17, -Big])), 17),
    ?ASSERT_MATCH(lists:sum(id([1, 2.5, -4])), -0.5),
    ?ASSERT_MATCH(lists:sum(id([0.0])), 0.0),
    %% Accumulate left-to-right, rather than reassociating float additions.
    ?ASSERT_MATCH(lists:sum(id([1.0e20, -1.0e20, 3.0])), 3.0),
    ?ASSERT_ERROR(lists:sum(id(not_a_list)), function_clause),
    ?ASSERT_ERROR(lists:sum(id([1 | bad_tail])), function_clause),
    ?ASSERT_ERROR(lists:sum(id([1, bad_number])), badarith),
    ?ASSERT_ERROR(lists:sum(id([bad_number | bad_tail])), badarith),
    ok.

id(X) ->
    X.
