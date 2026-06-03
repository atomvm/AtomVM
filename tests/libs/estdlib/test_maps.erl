%
% This file is part of AtomVM.
%
% Copyright 2021 Fred Dushin <fred@dushin.net>
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

-module(test_maps).

-export([test/0, id/1]).

-include("etest.hrl").

%% Map comprehensions (`#{K => V || K := V <- M}') were introduced in OTP 26.
-if(?OTP_RELEASE >= 26).
-define(HAS_MAP_COMPREHENSION, true).
-endif.

test() ->
    ok = test_get(),
    ok = test_is_key(),
    ok = test_put(),
    ok = test_iterator(),
    HasIterator2 =
        case erlang:system_info(machine) of
            "BEAM" ->
                erlang:system_info(version) >= "14.";
            "ATOM" ->
                true
        end,
    case HasIterator2 of
        true ->
            ok = test_iterator_2_undefined(),
            ok = test_iterator_2_ordered(),
            ok = test_iterator_2_reversed(),
            ok = test_iterator_2_f();
        false ->
            ok
    end,
    ok = test_keys(),
    ok = test_values(),
    ok = test_to_list(),
    ok = test_from_list(),
    ok = test_size(),
    ok = test_find(),
    ok = test_filter(),
    ok = test_fold(),
    ok = test_foreach(),
    ok = test_map(),
    ok = test_merge(),
    HasMergeWith =
        case erlang:system_info(machine) of
            "BEAM" ->
                erlang:system_info(version) >= "12.3";
            "ATOM" ->
                true
        end,
    case HasMergeWith of
        true ->
            ok = test_merge_with();
        false ->
            ok
    end,
    ok = test_remove(),
    ok = test_update(),
    ok = test_comprehension(),
    ok = test_take(),
    ok = test_update_with_3(),
    ok = test_update_with_4(),
    ok = test_with(),
    ok = test_without(),
    ok = test_filtermap(),
    ok = test_intersect(),
    ok = test_intersect_with(),
    ok = test_groups_from_list(),
    ok = test_is_iterator_valid(),
    ok.

test_get() ->
    ?ASSERT_MATCH(maps:get(foo, id(#{foo => bar})), bar),
    ok = check_bad_map(fun() -> maps:get(bar, id(not_a_map)) end),
    ok = check_bad_key(fun() -> maps:get(bar, id(#{foo => bar})) end, bar),

    ?ASSERT_MATCH(maps:get(gnu, id(#{foo => bar}), gnat), gnat),
    ?ASSERT_ERROR(maps:get({hello}, id(#{foo => bar})), {badkey, {hello}}),
    ?ASSERT_ERROR(maps:get(gnu, id({hello})), {badmap, {hello}}),
    ok.

test_is_key() ->
    ?ASSERT_MATCH(maps:is_key(foo, id(#{foo => bar})), true),
    ok = check_bad_map(fun() -> maps:is_key(bar, id(not_a_map)) end),
    ?ASSERT_MATCH(maps:is_key(bar, id(#{foo => bar})), false),
    ?ASSERT_ERROR(maps:is_key(gnu, id({hello})), {badmap, {hello}}),
    ok.

test_put() ->
    ?ASSERT_MATCH(maps:put(foo, bar, id(#{})), #{foo => bar}),
    ?ASSERT_MATCH(maps:put(foo, bar, id(#{foo => bar})), #{foo => bar}),
    ?ASSERT_MATCH(maps:put(foo, tapas, id(#{foo => bar})), #{foo => tapas}),
    ?ASSERT_MATCH(maps:put(gnu, gnat, id(#{foo => bar})), #{foo => bar, gnu => gnat}),
    ok = check_bad_map(fun() -> maps:put(bar, tapas, id(not_a_map)) end),
    ok.

test_iterator() ->
    Map = #{c => 3, a => 1, b => 2},
    Iterator0 = maps:iterator(Map),
    {XK, XV, Iterator1} = maps:next(Iterator0),
    {YK, YV, Iterator2} = maps:next(Iterator1),
    {ZK, ZV, Iterator3} = maps:next(Iterator2),
    [{a, 1}, {b, 2}, {c, 3}] = lists:sort([{XK, XV}, {YK, YV}, {ZK, ZV}]),
    none = maps:next(Iterator3),
    none = maps:next(none),

    EmptyMap = maps:new(),
    EmptyIterator = maps:iterator(EmptyMap),
    none = maps:next(EmptyIterator),

    ok.

test_iterator_2_undefined() ->
    Map = #{c => 3, a => 1, b => 2},
    Iterator0 = maps:iterator(Map, undefined),
    {XK, XV, Iterator1} = maps:next(Iterator0),
    {YK, YV, Iterator2} = maps:next(Iterator1),
    {ZK, ZV, Iterator3} = maps:next(Iterator2),
    [{a, 1}, {b, 2}, {c, 3}] = lists:sort([{XK, XV}, {YK, YV}, {ZK, ZV}]),
    none = maps:next(Iterator3),
    none = maps:next(none),

    EmptyMap = maps:new(),
    EmptyIterator = maps:iterator(EmptyMap),
    none = maps:next(EmptyIterator),

    ok.

test_iterator_2_ordered() ->
    Map = #{c => 3, a => 1, b => 2},
    Iterator0 = maps:iterator(Map, ordered),
    {a, 1, Iterator1} = maps:next(Iterator0),
    {b, 2, Iterator2} = maps:next(Iterator1),
    {c, 3, Iterator3} = maps:next(Iterator2),
    none = maps:next(Iterator3),
    none = maps:next(none),

    EmptyMap = maps:new(),
    EmptyIterator = maps:iterator(EmptyMap),
    none = maps:next(EmptyIterator),

    ok.

test_iterator_2_reversed() ->
    Map = #{c => 3, a => 1, b => 2},
    Iterator0 = maps:iterator(Map, reversed),
    {c, 3, Iterator1} = maps:next(Iterator0),
    {b, 2, Iterator2} = maps:next(Iterator1),
    {a, 1, Iterator3} = maps:next(Iterator2),
    none = maps:next(Iterator3),
    none = maps:next(none),

    EmptyMap = maps:new(),
    EmptyIterator = maps:iterator(EmptyMap),
    none = maps:next(EmptyIterator),

    ok.

test_iterator_2_f_order(c, _) -> true;
test_iterator_2_f_order(a, b) -> true;
test_iterator_2_f_order(_, _) -> false.

test_iterator_2_f() ->
    Map = #{c => 3, a => 1, b => 2},
    Iterator0 = maps:iterator(Map, fun test_iterator_2_f_order/2),
    {c, 3, Iterator1} = maps:next(Iterator0),
    {a, 1, Iterator2} = maps:next(Iterator1),
    {b, 2, Iterator3} = maps:next(Iterator2),
    none = maps:next(Iterator3),
    none = maps:next(none),

    EmptyMap = maps:new(),
    EmptyIterator = maps:iterator(EmptyMap),
    none = maps:next(EmptyIterator),

    ok.

test_keys() ->
    ?ASSERT_MATCH(maps:keys(maps:new()), []),
    ?ASSERT_MATCH(lists:sort(maps:keys(#{a => 1, b => 2, c => 3})), [a, b, c]),
    ok = check_bad_map(fun() -> maps:keys(id(not_a_map)) end),
    ok.

test_values() ->
    ?ASSERT_MATCH(maps:values(maps:new()), []),
    ?ASSERT_MATCH(lists:sort(maps:values(#{a => 1, b => 2, c => 3})), [1, 2, 3]),
    ok = check_bad_map(fun() -> maps:values(id(not_a_map)) end),
    ok.

test_to_list() ->
    ?ASSERT_MATCH(maps:to_list(maps:new()), []),
    ?ASSERT_MATCH(lists:sort(maps:to_list(#{a => 1, b => 2, c => 3})), [{a, 1}, {b, 2}, {c, 3}]),
    ok = check_bad_map(fun() -> maps:to_list(id(not_a_map)) end),
    ok.

test_from_list() ->
    ?ASSERT_EQUALS(maps:from_list([]), #{}),
    ?ASSERT_EQUALS(maps:from_list([{a, 1}, {b, 2}, {c, 3}]), #{a => 1, b => 2, c => 3}),
    ?ASSERT_ERROR(maps:from_list(id(foo)), badarg),
    ?ASSERT_ERROR(maps:from_list(id([improper | list])), badarg),
    ok.

test_size() ->
    ?ASSERT_MATCH(maps:size(maps:new()), 0),
    ?ASSERT_MATCH(maps:size(#{a => 1, b => 2, c => 3}), 3),
    ?ASSERT_ERROR(maps:size({hello}), {badmap, {hello}}),
    ok = check_bad_map(fun() -> maps:size(id(not_a_map)) end),
    ok.

test_find() ->
    ?ASSERT_MATCH(maps:find(foo, maps:new()), error),
    ?ASSERT_MATCH(maps:find(a, #{a => 1, b => 2, c => 3}), {ok, 1}),
    ?ASSERT_MATCH(maps:find(b, #{a => 1, b => 2, c => 3}), {ok, 2}),
    ?ASSERT_MATCH(maps:find(c, #{a => 1, b => 2, c => 3}), {ok, 3}),
    ?ASSERT_MATCH(maps:find(foo, #{a => 1, b => 2, c => 3}), error),
    ok = check_bad_map(fun() -> maps:find(foo, id(not_a_map)) end),
    ok.

test_filter() ->
    Filter = fun(_Key, Value) -> Value rem 2 == 0 end,
    ?ASSERT_EQUALS(maps:filter(Filter, maps:new()), #{}),
    ?ASSERT_EQUALS(maps:filter(Filter, #{a => 1, b => 2, c => 3}), #{b => 2}),
    ok = check_bad_map(fun() -> maps:filter(Filter, id(not_a_map)) end),
    ok = check_bad_map_or_badarg(fun() -> maps:filter(not_a_function, id(not_a_map)) end),
    ?ASSERT_ERROR(maps:filter(not_a_function, maps:new()), badarg),
    ok.

test_fold() ->
    Fun = fun(_Key, Value, Sum) -> Sum + Value end,
    ?ASSERT_EQUALS(maps:fold(Fun, 0, maps:new()), 0),
    ?ASSERT_EQUALS(maps:fold(Fun, 0, #{a => 1, b => 2, c => 3}), 6),
    ok = check_bad_map(fun() -> maps:fold(Fun, any, id(not_a_map)) end),
    ok = check_bad_map_or_badarg(fun() -> maps:fold(not_a_function, any, id(not_a_map)) end),
    ?ASSERT_ERROR(maps:fold(not_a_function, any, maps:new()), badarg),
    ok.

collect_foreach(Acc) ->
    Self = self(),
    receive
        {Self, Key, Value} ->
            collect_foreach([{Key, Value} | Acc])
    after 0 -> Acc
    end.

test_foreach() ->
    % maps:foreach/2 was introduced with OTP 24.
    HasForeach =
        case erlang:system_info(machine) of
            "BEAM" -> erlang:function_exported(maps, foreach, 2);
            "ATOM" -> true
        end,
    if
        HasForeach ->
            Self = self(),
            Fun = fun(Key, Value) -> Self ! {self(), Key, Value} end,
            ok = maps:foreach(Fun, maps:new()),
            ?ASSERT_EQUALS(collect_foreach([]), []),
            ok =
                receive
                    {Self, _, _} -> fail
                after 0 -> ok
                end,
            ok = maps:foreach(Fun, #{a => 1, b => 2, c => 3}),
            ?ASSERT_EQUALS(lists:sort(collect_foreach([])), [{a, 1}, {b, 2}, {c, 3}]),
            ok = check_bad_map(fun() -> maps:foreach(Fun, id(not_a_map)) end),
            ok = check_bad_map_or_badarg(fun() -> maps:foreach(not_a_function, id(not_a_map)) end),
            ?ASSERT_ERROR(maps:foreach(not_a_function, maps:new()), badarg),
            ok;
        true ->
            ok
    end.

test_map() ->
    Fun = fun(_Key, Value) -> 2 * Value end,
    ?ASSERT_EQUALS(maps:map(Fun, maps:new()), #{}),
    ?ASSERT_EQUALS(maps:map(Fun, #{a => 1, b => 2, c => 3}), #{a => 2, b => 4, c => 6}),
    ok = check_bad_map(fun() -> maps:map(Fun, id(not_a_map)) end),
    ok = check_bad_map_or_badarg(fun() -> maps:map(not_a_function, id(not_a_map)) end),
    ?ASSERT_ERROR(maps:map(not_a_function, maps:new()), badarg),
    ok.

test_merge() ->
    ?ASSERT_EQUALS(maps:merge(maps:new(), maps:new()), #{}),
    ?ASSERT_EQUALS(maps:merge(#{a => 1, b => 2, c => 3}, maps:new()), #{a => 1, b => 2, c => 3}),
    ?ASSERT_EQUALS(maps:merge(maps:new(), #{a => 1, b => 2, c => 3}), #{a => 1, b => 2, c => 3}),
    ?ASSERT_EQUALS(maps:merge(#{a => 1, b => 2, c => 3}, #{b => z, d => 4}), #{
        a => 1,
        b => z,
        c => 3,
        d => 4
    }),
    ok = check_bad_map(fun() -> maps:merge(maps:new(), id(not_a_map)) end),
    ok = check_bad_map(fun() -> maps:merge(id(not_a_map), maps:new()) end),
    ok.

test_merge_with() ->
    ?ASSERT_EQUALS(maps:merge_with(fun(_K, V1, V2) -> V1 + V2 end, maps:new(), maps:new()), #{}),
    ?ASSERT_EQUALS(
        maps:merge_with(fun(_K, V1, V2) -> V1 + V2 end, #{a => 1, b => 2, c => 3}, maps:new()), #{
            a => 1, b => 2, c => 3
        }
    ),
    ?ASSERT_EQUALS(
        maps:merge_with(fun(_K, V1, V2) -> V1 + V2 end, maps:new(), #{a => 1, b => 2, c => 3}), #{
            a => 1, b => 2, c => 3
        }
    ),
    ?ASSERT_EQUALS(
        maps:merge_with(fun(_K, V1, V2) -> V1 + V2 end, #{a => 1, b => 2, d => 4}, #{
            a => 1, b => 2, c => 3
        }),
        #{a => 2, b => 4, c => 3, d => 4}
    ),
    ?ASSERT_EQUALS(
        maps:merge_with(fun(_K, V1, V2) -> {V1, V2} end, #{a => 1, b => 2, c => 3}, #{
            b => z, d => 4
        }),
        #{
            a => 1,
            b => {2, z},
            c => 3,
            d => 4
        }
    ),
    ok = check_bad_map(fun() ->
        maps:merge_with(fun(_K, V1, V2) -> V1 + V2 end, maps:new(), id(not_a_map))
    end),
    ok = check_bad_map(fun() ->
        maps:merge_with(fun(_K, V1, V2) -> V1 + V2 end, id(not_a_map), maps:new())
    end),
    ok.

test_remove() ->
    ?ASSERT_EQUALS(maps:remove(foo, maps:new()), #{}),
    ?ASSERT_EQUALS(maps:remove(a, #{a => 1, b => 2, c => 3}), #{b => 2, c => 3}),
    ?ASSERT_EQUALS(maps:remove(b, #{a => 1, b => 2, c => 3}), #{a => 1, c => 3}),
    ?ASSERT_EQUALS(maps:remove(c, #{a => 1, b => 2, c => 3}), #{a => 1, b => 2}),
    ?ASSERT_EQUALS(maps:remove(d, #{a => 1, b => 2, c => 3}), #{a => 1, b => 2, c => 3}),
    ok = check_bad_map(fun() -> maps:remove(foo, id(not_a_map)) end),
    ok.

test_update() ->
    ?ASSERT_ERROR(maps:update(foo, bar, maps:new()), {badkey, foo}),
    ?ASSERT_EQUALS(maps:update(a, 10, #{a => 1, b => 2, c => 3}), #{a => 10, b => 2, c => 3}),
    ?ASSERT_EQUALS(maps:update(b, 20, #{a => 1, b => 2, c => 3}), #{a => 1, b => 20, c => 3}),
    ?ASSERT_EQUALS(maps:update(c, 30, #{a => 1, b => 2, c => 3}), #{a => 1, b => 2, c => 30}),
    ?ASSERT_ERROR(maps:update(d, 40, #{a => 1, b => 2, c => 3}), {badkey, d}),
    ?ASSERT_ERROR(maps:update({hello}, 40, #{a => 1, b => 2, c => 3}), {badkey, {hello}}),
    ?ASSERT_ERROR(maps:update(a, 40, {hello}), {badmap, {hello}}),
    ok = check_bad_map(fun() -> maps:update(foo, bar, id(not_a_map)) end),
    ok.

-ifdef(HAS_MAP_COMPREHENSION).
%% Map comprehensions rely on the compiler-generated calls to
%% erts_internal:mc_iterator/1 and erts_internal:mc_refill/1.
test_comprehension() ->
    %% Empty map yields an empty map.
    EmptyResult = #{K => V * 10 || K := V <- id(#{})},
    ?ASSERT_MATCH(map_size(EmptyResult), 0),

    %% Single and multiple associations, with a value transform.
    ?ASSERT_MATCH(#{K => V * 10 || K := V <- id(#{a => 1})}, #{a => 10}),
    ?ASSERT_MATCH(
        #{K => V * 10 || K := V <- id(#{a => 1, b => 2, c => 3})},
        #{a => 10, b => 20, c => 30}
    ),

    %% A filter drops associations.
    ?ASSERT_MATCH(
        #{K => V || K := V <- id(#{a => 1, b => 2, c => 3, d => 4}), V rem 2 =:= 0},
        #{b => 2, d => 4}
    ),

    %% Both key and value can be transformed.
    ?ASSERT_MATCH(
        #{{key, K} => V + 1 || K := V <- id(#{1 => 10, 2 => 20})},
        #{{key, 1} => 11, {key, 2} => 21}
    ),

    %% A map comprehension may iterate over a map iterator, not only a map.
    Iter = maps:iterator(id(#{x => 1, y => 2})),
    ?ASSERT_MATCH(#{K => V * 2 || K := V <- Iter}, #{x => 2, y => 4}),

    %% A map generator may also feed a list comprehension.
    ?ASSERT_MATCH(
        lists:sort([{K, V} || K := V <- id(#{a => 1, b => 2})]),
        [{a, 1}, {b, 2}]
    ),

    ok.
-else.
test_comprehension() ->
    ok.
-endif.
test_take() ->
    ?ASSERT_EQUALS(maps:take(foo, maps:new()), error),
    ?ASSERT_EQUALS(maps:take(a, #{a => 1, b => 2, c => 3}), {1, #{b => 2, c => 3}}),
    ?ASSERT_EQUALS(maps:take(b, #{a => 1, b => 2, c => 3}), {2, #{a => 1, c => 3}}),
    ?ASSERT_EQUALS(maps:take(c, #{a => 1, b => 2, c => 3}), {3, #{a => 1, b => 2}}),
    ?ASSERT_EQUALS(maps:take(d, #{a => 1, b => 2, c => 3}), error),
    %% value happens to be the atom 'error' - must not be confused with missing-key result
    ?ASSERT_EQUALS(maps:take(a, #{a => error, b => 2}), {error, #{b => 2}}),
    %% taking the only key leaves an empty map
    ?ASSERT_EQUALS(maps:take(a, #{a => 1}), {1, #{}}),
    ok = check_bad_map(fun() -> maps:take(foo, id(not_a_map)) end),
    ok.

test_update_with_3() ->
    Inc = fun(V) -> V + 1 end,
    ?ASSERT_EQUALS(maps:update_with(a, Inc, #{a => 1, b => 2}), #{a => 2, b => 2}),
    ?ASSERT_EQUALS(maps:update_with(b, Inc, #{a => 1, b => 2}), #{a => 1, b => 3}),
    ?ASSERT_ERROR(maps:update_with(c, Inc, #{a => 1, b => 2}), {badkey, c}),
    ok = check_bad_map(fun() -> maps:update_with(a, Inc, id(not_a_map)) end),
    ?ASSERT_ERROR(maps:update_with(a, not_a_function, maps:new()), badarg),
    %% wrong-arity fun also yields badarg
    ?ASSERT_ERROR(maps:update_with(a, fun(_, _) -> ok end, maps:new()), badarg),
    %% badmap takes precedence over badarg when both args are wrong
    ok = check_bad_map(fun() -> maps:update_with(a, not_a_function, id(not_a_map)) end),
    ok.

test_update_with_4() ->
    Inc = fun(V) -> V + 1 end,
    ?ASSERT_EQUALS(maps:update_with(a, Inc, 0, #{a => 1, b => 2}), #{a => 2, b => 2}),
    ?ASSERT_EQUALS(maps:update_with(b, Inc, 0, #{a => 1, b => 2}), #{a => 1, b => 3}),
    ?ASSERT_EQUALS(maps:update_with(c, Inc, 42, #{a => 1, b => 2}), #{a => 1, b => 2, c => 42}),
    ?ASSERT_EQUALS(maps:update_with(c, Inc, 42, maps:new()), #{c => 42}),
    ok = check_bad_map(fun() -> maps:update_with(a, Inc, 0, id(not_a_map)) end),
    ?ASSERT_ERROR(maps:update_with(a, not_a_function, 0, maps:new()), badarg),
    %% Fun must NOT be invoked when inserting Init for a missing key
    Crash = fun(_) -> error(should_not_be_called) end,
    ?ASSERT_EQUALS(maps:update_with(new_key, Crash, init, #{}), #{new_key => init}),
    %% wrong-arity fun also yields badarg
    ?ASSERT_ERROR(maps:update_with(a, fun(_, _) -> ok end, 0, maps:new()), badarg),
    %% badmap takes precedence over badarg when both args are wrong
    ok = check_bad_map(fun() -> maps:update_with(a, not_a_function, 0, id(not_a_map)) end),
    ok.

test_with() ->
    ?ASSERT_EQUALS(maps:with([], #{a => 1, b => 2, c => 3}), #{}),
    ?ASSERT_EQUALS(maps:with([a, c], #{a => 1, b => 2, c => 3}), #{a => 1, c => 3}),
    ?ASSERT_EQUALS(maps:with([a, missing], #{a => 1, b => 2, c => 3}), #{a => 1}),
    ?ASSERT_EQUALS(maps:with([missing], #{a => 1, b => 2, c => 3}), #{}),
    ?ASSERT_EQUALS(maps:with([a, b, c], maps:new()), #{}),
    %% duplicate keys are tolerated
    ?ASSERT_EQUALS(maps:with([a, a, c], #{a => 1, b => 2, c => 3}), #{a => 1, c => 3}),
    ok = check_bad_map(fun() -> maps:with([a], id(not_a_map)) end),
    ?ASSERT_ERROR(maps:with(id(not_a_list), maps:new()), badarg),
    %% badmap takes precedence over badarg when both args are wrong
    ok = check_bad_map(fun() -> maps:with(id(not_a_list), id(not_a_map)) end),
    ok.

test_without() ->
    ?ASSERT_EQUALS(maps:without([], #{a => 1, b => 2, c => 3}), #{a => 1, b => 2, c => 3}),
    ?ASSERT_EQUALS(maps:without([a, c], #{a => 1, b => 2, c => 3}), #{b => 2}),
    ?ASSERT_EQUALS(maps:without([missing], #{a => 1, b => 2, c => 3}), #{a => 1, b => 2, c => 3}),
    ?ASSERT_EQUALS(maps:without([a, b, c], #{a => 1, b => 2, c => 3}), #{}),
    ?ASSERT_EQUALS(maps:without([a], maps:new()), #{}),
    %% duplicate keys are tolerated
    ?ASSERT_EQUALS(maps:without([a, a, c], #{a => 1, b => 2, c => 3}), #{b => 2}),
    ok = check_bad_map(fun() -> maps:without([a], id(not_a_map)) end),
    ?ASSERT_ERROR(maps:without(id(not_a_list), maps:new()), badarg),
    %% badmap takes precedence over badarg when both args are wrong
    ok = check_bad_map(fun() -> maps:without(id(not_a_list), id(not_a_map)) end),
    ok.

test_filtermap() ->
    %% Empty map yields empty map
    ?ASSERT_EQUALS(maps:filtermap(fun(_K, _V) -> true end, maps:new()), #{}),
    %% Keep all entries
    ?ASSERT_EQUALS(
        maps:filtermap(fun(_K, _V) -> true end, #{a => 1, b => 2}),
        #{a => 1, b => 2}
    ),
    %% Drop all entries
    ?ASSERT_EQUALS(
        maps:filtermap(fun(_K, _V) -> false end, #{a => 1, b => 2}),
        #{}
    ),
    %% Filter with predicate
    ?ASSERT_EQUALS(
        maps:filtermap(fun(_K, V) -> V > 1 end, #{a => 1, b => 2, c => 3}),
        #{b => 2, c => 3}
    ),
    %% Transform values
    ?ASSERT_EQUALS(
        maps:filtermap(fun(_K, V) -> {true, V * 10} end, #{a => 1, b => 2}),
        #{a => 10, b => 20}
    ),
    %% Filter and transform
    ?ASSERT_EQUALS(
        maps:filtermap(fun(_K, V) -> case V rem 2 of 0 -> {true, V * 10}; _ -> false end end, #{
            a => 1, b => 2, c => 3, d => 4
        }),
        #{b => 20, d => 40}
    ),
    %% Works with iterators
    Iter = maps:iterator(#{a => 1, b => 2, c => 3}),
    ?ASSERT_EQUALS(
        maps:filtermap(fun(_K, V) -> V > 1 end, Iter),
        #{b => 2, c => 3}
    ),
    %% Error cases
    ok = check_bad_map(fun() -> maps:filtermap(fun(_K, _V) -> true end, id(not_a_map)) end),
    ?ASSERT_ERROR(maps:filtermap(not_a_function, maps:new()), badarg),
    ok.

test_intersect() ->
    %% Empty maps
    ?ASSERT_EQUALS(maps:intersect(maps:new(), maps:new()), #{}),
    ?ASSERT_EQUALS(maps:intersect(#{a => 1}, maps:new()), #{}),
    ?ASSERT_EQUALS(maps:intersect(maps:new(), #{a => 1}), #{}),
    %% No common keys
    ?ASSERT_EQUALS(maps:intersect(#{a => 1}, #{b => 2}), #{}),
    %% Some common keys - values from second map
    ?ASSERT_EQUALS(
        maps:intersect(#{a => 1, b => 2, c => 3}, #{a => 10, b => 20, d => 40}),
        #{a => 10, b => 20}
    ),
    %% All keys common
    ?ASSERT_EQUALS(
        maps:intersect(#{a => 1, b => 2}, #{a => 10, b => 20}),
        #{a => 10, b => 20}
    ),
    %% Error cases
    ok = check_bad_map(fun() -> maps:intersect(id(not_a_map), #{}) end),
    ok = check_bad_map(fun() -> maps:intersect(#{}, id(not_a_map)) end),
    ok.

test_intersect_with() ->
    %% Empty maps
    Combiner = fun(_K, V1, V2) -> V1 + V2 end,
    ?ASSERT_EQUALS(maps:intersect_with(Combiner, maps:new(), maps:new()), #{}),
    ?ASSERT_EQUALS(maps:intersect_with(Combiner, #{a => 1}, maps:new()), #{}),
    ?ASSERT_EQUALS(maps:intersect_with(Combiner, maps:new(), #{a => 1}), #{}),
    %% No common keys
    ?ASSERT_EQUALS(maps:intersect_with(Combiner, #{a => 1}, #{b => 2}), #{}),
    %% Some common keys - combine values
    ?ASSERT_EQUALS(
        maps:intersect_with(Combiner, #{a => 1, b => 2, c => 3}, #{a => 10, b => 20, d => 40}),
        #{a => 11, b => 22}
    ),
    %% All keys common
    ?ASSERT_EQUALS(
        maps:intersect_with(Combiner, #{a => 1, b => 2}, #{a => 10, b => 20}),
        #{a => 11, b => 22}
    ),
    %% Combiner receives key and both values
    KeyCombiner = fun(K, V1, V2) -> {K, V1, V2} end,
    ?ASSERT_EQUALS(
        maps:intersect_with(KeyCombiner, #{a => 1}, #{a => 2}),
        #{a => {a, 1, 2}}
    ),
    %% Map1 larger than Map2 still passes values to Combiner in Map1, Map2 order
    ?ASSERT_EQUALS(
        maps:intersect_with(KeyCombiner, #{a => 1, b => 2, c => 3}, #{b => 20}),
        #{b => {b, 2, 20}}
    ),
    %% Error cases
    ok = check_bad_map(fun() -> maps:intersect_with(Combiner, id(not_a_map), #{}) end),
    ok = check_bad_map(fun() -> maps:intersect_with(Combiner, #{}, id(not_a_map)) end),
    ?ASSERT_ERROR(maps:intersect_with(not_a_function, #{}, #{}), badarg),
    ok.

test_groups_from_list() ->
    %% Empty list
    ?ASSERT_EQUALS(maps:groups_from_list(fun(X) -> X end, []), #{}),
    %% Group by identity
    ?ASSERT_EQUALS(
        maps:groups_from_list(fun(X) -> X end, [a, b, a, c, b, a]),
        #{a => [a, a, a], b => [b, b], c => [c]}
    ),
    %% Group by length
    ?ASSERT_EQUALS(
        maps:groups_from_list(fun length/1, ["ant", "buffalo", "cat", "dingo"]),
        #{3 => ["ant", "cat"], 5 => ["dingo"], 7 => ["buffalo"]}
    ),
    %% With value transformation
    ?ASSERT_EQUALS(
        maps:groups_from_list(fun(X) -> X rem 2 end, fun(X) -> X * 10 end, [1, 2, 3, 4, 5]),
        #{0 => [20, 40], 1 => [10, 30, 50]}
    ),
    %% Preserves order within groups
    ?ASSERT_EQUALS(
        maps:groups_from_list(fun(X) -> X rem 3 end, [1, 2, 3, 4, 5, 6]),
        #{0 => [3, 6], 1 => [1, 4], 2 => [2, 5]}
    ),
    %% Error cases
    ?ASSERT_ERROR(maps:groups_from_list(not_a_function, []), badarg),
    ?ASSERT_ERROR(maps:groups_from_list(fun(X) -> X end, not_a_list), badarg),
    ok.

test_is_iterator_valid() ->
    %% Valid iterator from empty map
    Iter1 = maps:iterator(maps:new()),
    ?ASSERT_EQUALS(maps:is_iterator_valid(Iter1), true),
    %% Valid iterator from non-empty map
    Iter2 = maps:iterator(#{a => 1, b => 2}),
    ?ASSERT_EQUALS(maps:is_iterator_valid(Iter2), true),
    %% Exhausted iterator (none) is valid
    ?ASSERT_EQUALS(maps:is_iterator_valid(none), true),
    %% Partially consumed iterator
    {_, _, Iter3} = maps:next(maps:iterator(#{a => 1, b => 2})),
    ?ASSERT_EQUALS(maps:is_iterator_valid(Iter3), true),
    %% Ordered iterator
    Iter4 = maps:iterator(#{a => 1, b => 2}, ordered),
    ?ASSERT_EQUALS(maps:is_iterator_valid(Iter4), true),
    %% Invalid iterators
    ?ASSERT_EQUALS(maps:is_iterator_valid(not_an_iterator), false),
    ?ASSERT_EQUALS(maps:is_iterator_valid(42), false),
    ?ASSERT_EQUALS(maps:is_iterator_valid([]), false),
    ?ASSERT_EQUALS(maps:is_iterator_valid([not_int | #{}]), false),
    ok.

id(X) -> X.

check_bad_map(F) ->
    try
        F(),
        fail
    catch
        error:{badmap, _} -> ok
    end.

check_bad_map_or_badarg(F) ->
    BadargFirst =
        case erlang:system_info(machine) of
            "BEAM" -> erlang:system_info(version) >= "12.";
            "ATOM" -> false
        end,
    try
        F(),
        fail
    catch
        error:{badmap, _} when not BadargFirst -> ok;
        error:badarg when BadargFirst -> ok
    end.

check_bad_key(F, _Key) ->
    try
        F(),
        fail
    catch
        error:{badkey, _} ->
            ok
    end.
