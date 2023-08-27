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

test() ->
    ok = test_get(),
    ok = test_is_key(),
    ok = test_put(),
    ok = test_iterator(),
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
    ok = test_remove(),
    ok = test_update(),
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
        %% TODO OTP23 compiler compiles to erlang:map_ equivalents
        error:{badkey, _} ->
            ok
    end.
