%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M. <petermm@gmail.com>
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

-module(test_persistent_term).

-export([start/0, map_update_worker/2, insert_worker/3]).

start() ->
    ok = test_get_put(),
    % put_new/2 is OTP 28.4+ only
    case erlang:function_exported(persistent_term, put_new, 2) of
        true -> ok = test_put_new();
        false -> ok
    end,
    ok = test_complex_keys(),
    ok = test_fun_keys(),
    ok = test_unresolved_fun_keys(),
    ok = test_persistent_map_exact_update(),
    ok = test_info_and_get_all(),
    ok = test_concurrent_insert(),
    0.

test_get_put() ->
    Key = {?MODULE, basic},
    assert_badarg(fun() -> persistent_term:get(Key) end),
    default = persistent_term:get(Key, default),
    Value = {stored, [<<1:80/unit:8>>, <<"small">>]},
    ok = persistent_term:put(Key, Value),
    Value = persistent_term:get(Key),
    #{count := Count, memory := Memory} = persistent_term:info(),
    ok = persistent_term:put(Key, Value),
    case erlang:system_info(machine) of
        "ATOM" ->
            false = erlang:function_exported(persistent_term, erase, 1),
            assert_badarg(fun() -> persistent_term:put(Key, {new, value}) end),
            % A rejected replacement must not attempt to copy this huge tree.
            Shared = make_shared_tree(40),
            assert_badarg(fun() -> persistent_term:put(Key, Shared) end);
        _ ->
            ok
    end,
    #{count := Count, memory := Memory} = persistent_term:info(),
    true = erlang:garbage_collect(),
    Value = persistent_term:get(Key),
    ok.

test_put_new() ->
    Key = {?MODULE, put_new},
    ok = persistent_term:put_new(Key, first),
    ok = persistent_term:put_new(Key, first),
    first = persistent_term:get(Key),
    assert_badarg(fun() -> persistent_term:put_new(Key, second) end),
    first = persistent_term:get(Key),
    Shared = make_shared_tree(40),
    assert_badarg(fun() -> persistent_term:put_new(Key, Shared) end),
    first = persistent_term:get(Key),
    ok.

test_complex_keys() ->
    Key = {{?MODULE, complex}, [self(), <<"bin">>], #{a => 1, <<"b">> => {c, d}}},
    ok = persistent_term:put(Key, complex_value),
    complex_value = persistent_term:get(Key),
    complex_value = persistent_term:get(Key, complex_value),
    ok.

test_fun_keys() ->
    LocalFun = fun identity/1,
    ExternalFun = fun erlang:length/1,
    Key = {?MODULE, fun_key, LocalFun, ExternalFun},
    EquivalentKey = {?MODULE, fun_key, fun identity/1, fun erlang:length/1},
    ok = persistent_term:put(Key, fun_value),
    fun_value = persistent_term:get(EquivalentKey),
    fun_value = persistent_term:get(Key),
    ok.

test_unresolved_fun_keys() ->
    Fun1 = make_unresolved_fun(42),
    Fun2 = make_unresolved_fun(42),
    Fun3 = make_unresolved_fun(43),
    true = Fun1 =:= Fun2,
    false = Fun1 =:= Fun3,
    Key1 = {?MODULE, unresolved_fun, Fun1},
    Key2 = {?MODULE, unresolved_fun, Fun2},
    ok = persistent_term:put(Key1, unresolved_fun_value),
    unresolved_fun_value = persistent_term:get(Key2),
    ok.

test_persistent_map_exact_update() ->
    PersistentKey = {?MODULE, persistent_map},
    MapKey = {compound_key, seq(1, 64)},
    ok = persistent_term:put(PersistentKey, #{MapKey => original}),
    {Pid, Ref} = spawn_opt(?MODULE, map_update_worker, [PersistentKey, self()], [monitor]),
    receive
        updated -> ok
    end,
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok
    end,
    churn(100),
    true = erlang:garbage_collect(),
    PersistentMap = persistent_term:get(PersistentKey),
    original = maps:get({compound_key, seq(1, 64)}, PersistentMap),
    ok.

test_info_and_get_all() ->
    Key1 = {?MODULE, info_1},
    Key2 = {?MODULE, info_2},
    #{count := Count0} = persistent_term:info(),

    ok = persistent_term:put(Key1, value1),
    ok = persistent_term:put(Key2, {value2, [1, 2, 3]}),

    #{count := Count1, memory := Memory1} = persistent_term:info(),
    true = Count1 >= Count0 + 2,
    true = is_integer(Memory1),
    true = Memory1 > 0,

    ok = persistent_term:put(Key1, value1),
    #{count := Count1, memory := Memory1} = persistent_term:info(),
    All = persistent_term:get(),
    true = erlang:garbage_collect(),
    true = member({Key1, value1}, All),
    true = member({Key2, {value2, [1, 2, 3]}}, All),
    ok.

test_concurrent_insert() ->
    % All writers race on one key. Exactly one distinct value can be stored.
    case erlang:system_info(machine) of
        "ATOM" ->
            Key = {?MODULE, concurrent},
            #{count := Count0} = persistent_term:info(),
            Parent = self(),
            Workers = [
                spawn_opt(?MODULE, insert_worker, [Key, N, Parent], [monitor])
             || N <- seq(1, 16)
            ],
            [Pid ! go || {Pid, _} <- Workers],
            Results = [
                receive
                    {inserted, Pid, Result} -> Result
                end
             || {Pid, _} <- Workers
            ],
            [
                receive
                    {'DOWN', Ref, process, Pid, normal} -> ok
                end
             || {Pid, Ref} <- Workers
            ],
            [Winner] = [N || {ok, N} <- Results],
            Winner = persistent_term:get(Key),
            15 = length([rejected || rejected <- Results]),
            #{count := Count1} = persistent_term:info(),
            Count1 = Count0 + 1,
            ok;
        _ ->
            ok
    end.

insert_worker(Key, Value, Parent) ->
    receive
        go -> ok
    end,
    Result =
        try persistent_term:put(Key, Value) of
            ok -> {ok, Value}
        catch
            error:badarg -> rejected
        end,
    Parent ! {inserted, self(), Result}.

make_unresolved_fun(Capture) ->
    MissingModule = <<"persistent_term_unresolved_fun_module">>,
    MissingModuleLen = byte_size(MissingModule),
    PidNode = atom_to_binary(?MODULE, utf8),
    PidNodeLen = byte_size(PidNode),
    Body =
        <<119, MissingModuleLen, MissingModule/binary, 97, 0, 97, 0, 88, 119, PidNodeLen,
            PidNode/binary, 0:32, 0:32, 0:32, 97, Capture>>,
    Rest = <<0, 0:128, 0:32, 1:32, Body/binary>>,
    Size = byte_size(Rest) + 4,
    binary_to_term(<<131, 112, Size:32, Rest/binary>>).

make_shared_tree(0) ->
    leaf;
make_shared_tree(N) ->
    Child = make_shared_tree(N - 1),
    {Child, Child}.

churn(0) ->
    ok;
churn(N) ->
    _ = seq(1, 128),
    churn(N - 1).

map_update_worker(Key, Parent) ->
    Map = persistent_term:get(Key),
    EqualKey = {compound_key, seq(1, 64)},
    Updated = Map#{EqualKey := changed},
    changed = maps:get(EqualKey, Updated),
    Parent ! updated.

assert_badarg(Fun) ->
    {'EXIT', {badarg, _}} = (catch Fun()),
    ok.

identity(Value) ->
    Value.

seq(First, Last) when First > Last -> [];
seq(First, Last) -> [First | seq(First + 1, Last)].

member(Value, [Value | _]) -> true;
member(Value, [_ | Rest]) -> member(Value, Rest);
member(_Value, []) -> false.
