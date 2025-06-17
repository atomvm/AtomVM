%
% This file is part of AtomVM.
%
% Copyright 2024 Fred Dushin <fred@dushin.net>
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

-module(test_ets).

-export([start/0]).

start() ->
    % ok = isolated(fun test_ets_new/0),
    % ok = isolated(fun test_permissions/0),
    % ok = isolated(fun test_keys/0),
    % ok = isolated(fun test_keypos/0),
    % ok = isolated(fun test_insert/0),
    % ok = isolated(fun test_delete/0),
    % ok = isolated(fun test_lookup_element/0),
    % ok = isolated(fun test_update_counter/0),
    ok = isolated(fun test_duplicate_bag/0),
    0.

test_ets_new() ->
    assert_badarg(fun() -> ets:new([isnt, atom], []) end),
    assert_badarg(fun() -> ets:new(name, not_a_list) end),

    ets:new(unnamed_test, []),

    named_test = ets:new(named_test, [named_table]),
    true = assert_operation(named_test, insert, []),
    ets:new(named_test, []),
    assert_badarg(fun() -> ets:new(named_test, [named_table]) end),

    ets:new(keypos_test, [{keypos, 2}]),
    assert_badarg(fun() -> ets:new(keypos_test, [{keypos, 0}]) end),
    assert_badarg(fun() -> ets:new(keypos_test, [{keypos, -1}]) end),

    ets:new(type_test, [set]),
    ets:new(type_test, [duplicate_bag]),

    % Unimplemented in AtomVM
    Options = [
        fun() -> ets:new(type_test, [ordered_set]) end,
        fun() -> ets:new(type_test, [bag]) end,
        fun() -> ets:new(heir_test, [{heir, self(), []}]) end,
        fun() -> ets:new(heir_test, [{heir, none}]) end,
        fun() -> ets:new(write_conc_test, [{write_concurrency, true}]) end,
        fun() -> ets:new(read_conc_test, [{read_concurrency, true}]) end,
        fun() -> ets:new(compressed_test, [compressed]) end
    ],
    Otp23Options = [
        fun() -> ets:new(decent_counters_test, [{decentralized_counters, true}]) end | Options
    ],
    case vm_version() of
        atom -> [assert_badarg(NewF) || NewF <- Otp23Options];
        {otp, V} when V >= 23 -> [NewF() || NewF <- Otp23Options];
        {otp, _V} -> [NewF() || NewF <- Options]
    end,
    ok.

test_permissions() ->
    % Table doesn't exist after process dies
    TDead = isolated(fun() ->
        T = ets:new(test, []),
        assert_operation(T, insert, []),
        assert_operation(T, lookup, []),
        T
    end),
    assert_operation(TDead, insert, [badarg]),
    assert_operation(TDead, lookup, [badarg]),

    % Private
    TPriv = ets:new(test, [private]),
    assert_operation(TPriv, insert, []),
    assert_operation(TPriv, lookup, []),
    isolated(fun() ->
        assert_operation(TPriv, insert, [badarg]),
        assert_operation(TPriv, lookup, [badarg])
    end),

    % Protected (default)
    TProt = ets:new(test, []),
    assert_operation(TProt, insert, []),
    assert_operation(TProt, lookup, []),
    isolated(fun() ->
        assert_operation(TProt, insert, [badarg]),
        assert_operation(TProt, lookup, [])
    end),

    % Public
    TPub = ets:new(test, [public]),
    assert_operation(TPub, insert, []),
    assert_operation(TPub, lookup, []),
    isolated(fun() ->
        assert_operation(TPub, insert, []),
        assert_operation(TPub, lookup, [])
    end),
    ok.

test_keys() ->
    T = ets:new(test, []),
    DistributedPidBin = <<131, 88, 119, 14, "test@test_node", 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 42>>,
    DistributedRefBin = <<131, 90, 0, 2, 119, 14, "test@test_node", 43:32, 1:32, 2:32>>,
    DistributedPortBin =
        <<131, 120, 119, 14, "test@test_node", 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 4, 18>>,
    Port = open_port({spawn, "echo"}, []),
    ok = assert_stored_key(T, some_atom),
    ok = assert_stored_key(T, 1),
    ok = assert_stored_key(T, 0),
    ok = assert_stored_key(T, -1),
    ok = assert_stored_key(T, 1.0),
    ok = assert_stored_key(T, self()),
    ok = assert_stored_key(T, binary_to_term(DistributedPidBin)),
    ok = assert_stored_key(T, Port),
    ok = assert_stored_key(T, erlang:make_ref()),
    ok = assert_stored_key(T, binary_to_term(DistributedRefBin)),
    ok = assert_stored_key(T, <<"bin">>),
    ok = assert_stored_key(T, <<"">>),
    ok = assert_stored_key(T, {ok, 1}),
    ok = assert_stored_key(T, [x, self(), 1.0]),
    ok = assert_stored_key(T, [improper | list]),
    ok = assert_stored_key(T, #{
        some_atom => {a, b, c},
        #{another => "map"} => erlang:make_ref(),
        <<1, 2, 3, 4>> => <<-4, -3, -2, -1>>
    }),
    case supports_v4_port_encoding() of
        true -> ok = assert_stored_key(T, binary_to_term(DistributedPortBin));
        false -> ok
    end,
    ok.

test_keypos() ->
    T = ets:new(test, [{keypos, 2}]),
    assert_badarg(fun() -> ets:new(bad_keypos, 0) end),
    assert_badarg(fun() -> ets:new(bad_keypos, -1) end),

    true = ets:insert(T, {value, key}),
    [{value, key}] = ets:lookup(T, key),
    assert_badarg(fun() -> ets:insert(T, {}) end),
    assert_badarg(fun() -> ets:insert(T, {value}) end),
    ok.

test_insert() ->
    T = ets:new(test, []),
    [] = ets:lookup(T, key),
    true = ets:insert(T, {key, value}),
    [{key, value}] = ets:lookup(T, key),
    % Overwrite
    true = ets:insert(T, {key, new_value}),
    [{key, new_value}] = ets:lookup(T, key),

    TList = ets:new(test, []),
    true = ets:insert(TList, []),
    true = ets:insert(TList, [{key, value}, {key2, value2}]),
    true = ets:insert(TList, [{key2, new_value2}, {key3, value3}]),
    [{key, value}] = ets:lookup(TList, key),
    [{key2, new_value2}] = ets:lookup(TList, key2),
    [{key3, value3}] = ets:lookup(TList, key3),

    TErr = ets:new(test, []),
    assert_badarg(fun() -> ets:insert(TErr, {}) end),
    assert_badarg(fun() -> ets:insert(TErr, [{}]) end),
    assert_badarg(fun() -> ets:insert(TErr, [{key, value}, not_a_tuple]) end),
    assert_badarg(fun() -> ets:insert(TErr, [{improper, true} | {list, true}]) end),
    assert_badarg(fun() -> ets:insert(TErr, not_a_tuple) end),
    assert_badarg(fun() -> ets:insert([not_a_ref], {key, value}) end),
    [] = ets:lookup(TErr, key),
    [] = ets:lookup(TErr, improper),
    [] = ets:lookup(TErr, list),
    ok.

test_delete() ->
    T = ets:new(test, []),

    % Not existing
    true = ets:delete(T, key),
    [] = ets:lookup(T, key),

    % Keep key2
    true = ets:insert(T, {key, value}),
    true = ets:insert(T, {key2, value2}),
    [{key, value}] = ets:lookup(T, key),
    true = ets:delete(T, key),
    [] = ets:lookup(T, key),
    [{key2, value2}] = ets:lookup(T, key2),

    % Re-add key
    true = ets:insert(T, {key, new_value}),
    [{key, new_value}] = ets:lookup(T, key),

    % Drop entire table
    true = ets:delete(T),
    assert_badarg(fun() -> ets:lookup(T, key) end),
    assert_badarg(fun() -> ets:delete(T) end),
    assert_badarg(fun() -> ets:delete(none) end),
    ok.

test_lookup_element() ->
    T = ets:new(test, []),
    true = ets:insert(T, {key, value}),
    key = ets:lookup_element(T, key, 1),
    value = ets:lookup_element(T, key, 2),
    assert_badarg(fun() -> ets:lookup_element(T, none, 1) end),
    assert_badarg(fun() -> ets:lookup_element(T, key, 3) end),
    assert_badarg(fun() -> ets:lookup_element(T, key, 0) end),
    assert_badarg(fun() -> ets:lookup_element(T, key, -1) end),
    ok.

test_update_counter() ->
    T = ets:new(test, []),
    true = ets:insert(T, {key, 10, 20, 30}),
    % Increment
    15 = ets:update_counter(T, key, 5),
    10 = ets:update_counter(T, key, -5),
    % {Position, Increment}
    25 = ets:update_counter(T, key, {3, 5}),
    20 = ets:update_counter(T, key, {3, -5}),
    % {Position, Increment, Threshold, SetValue}
    31 = ets:update_counter(T, key, {4, 10, 39, 31}),
    30 = ets:update_counter(T, key, {4, -10, 30, 30}),

    % {Position, Increment} with non-default position
    T2 = ets:new(test, [{keypos, 2}]),
    true = ets:insert(T2, {100, 200}),
    150 = ets:update_counter(T2, 200, {1, 50}),

    TErr = ets:new(test, []),
    true = ets:insert(TErr, {key, 0, not_number}),
    true = ets:insert(TErr, {0}),
    true = ets:insert(TErr, {not_number, ok}),
    assert_badarg(fun() -> ets:update_counter(TErr, none, 10) end),
    assert_badarg(fun() -> ets:update_counter(TErr, not_number, 10) end),
    assert_badarg(fun() -> ets:update_counter(TErr, not_number, {1, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, not_number, {1, 10, 100, 0}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {0, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {-1, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {1, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, 0, {1, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {3, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, not_number) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {not_number, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {2, not_number}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {not_number, 10, 100, 0}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {2, not_number, 100, 0}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {2, 10, not_number, 0}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {2, 10, 100, not_number}) end),
    ok.

test_duplicate_bag() ->
    Tid = ets:new(test_duplicate_bag, [duplicate_bag, {keypos, 2}]),
    T = {ok, foo, 100, extra},
    T2 = {error, foo, 200},
    T3 = {error, foo, 300},

    % true = ets:insert_new(Tid, T),
    % false = ets:insert_new(Tid, T),
    true = ets:insert(Tid, T),
    true = ets:insert(Tid, T),
    true = ets:insert(Tid, [T, T]),
    true = ets:insert(Tid, [T2]),
    true = [T, T, T, T2] == ets:lookup(Tid, foo),
    % true = ets:member(Tid, foo),

    % % nothing inserted, T exists in table
    % false = ets:insert_new(Tid, [T, {ok, bar, batat}]),
    % false = ets:member(Tid, bar),

    % [ok, ok, ok, ok, error] = ets:lookup_element(Tid, foo, 1),
    % [foo, foo, foo, foo, foo] = ets:lookup_element(Tid, foo, 2),
    % [100, 100, 100, 100, 200] = ets:lookup_element(Tid, foo, 3),
    % % some tuples don't have 4 arity
    % ok = expect_failure(fun() -> ets:lookup_element(Tid, foo, 4) end),

    % % unsupported for duplicate bag
    % ok = expect_failure(fun() -> ets:update_counter(Tid, foo, 10) end),
    % ok = expect_failure(fun() -> ets:update_element(Tid, foo, {1, error}) end),

    % true = ets:delete_object(Tid, {bad, bad}),
    % true = [T, T, T, T, T2] == ets:lookup(Tid, foo),
    % true = ets:delete_object(Tid, T),
    % true = [T2] == ets:lookup(Tid, foo),

    % true = ets:insert(Tid, T3),
    % % keeps insertion order
    % true = [T2, T3] == ets:take(Tid, foo),

    % true = ets:delete(Tid),
    % ok = expect_failure(fun() -> ets:insert(Tid, T) end),

    ok.

%%-----------------------------------------------------------------------------
%% @doc Performs specified operation on ETS table implicitly asserting that no exception is raised.
%%      [badarg] can be passed as an option to assert that exception was raised.
%% @end
%%-----------------------------------------------------------------------------
assert_operation(T, Operation, Opts) ->
    Op =
        case Operation of
            insert -> fun() -> ets:insert(T, {key, value}) end;
            lookup -> fun() -> ets:lookup(T, key) end
        end,
    case Opts of
        [badarg] ->
            assert_badarg(Op);
        _ ->
            Op()
    end.

%%-----------------------------------------------------------------------------
%% @doc Asserts that key can be used for insertion and retrieval of a value.
%% @end
%%-----------------------------------------------------------------------------
assert_stored_key(T, Key) ->
    true = ets:insert(T, {Key, value}),
    [{Key, value}] = ets:lookup(T, Key),
    ok.

isolated(Fun) ->
    Ref = make_ref(),
    Self = self(),
    spawn_opt(
        fun() ->
            Ret = Fun(),
            Self ! {ok, Ref, Ret}
        end,
        []
    ),
    receive
        {ok, Ref, Ret} -> Ret
    after 500 -> {error, timeout}
    end.

assert_badarg(Fun) ->
    try Fun() of
        R -> erlang:error({no_throw, R})
    catch
        error:badarg ->
            ok;
        C:E ->
            erlang:error({C, E})
    end.

supports_v4_port_encoding() ->
    case vm_version() of
        % small utf8 atom
        atom -> true;
        {otp, V} when V < 24 -> false;
        % v4 is supported but not the default
        {otp, V} when V < 26 -> true;
        % small utf8 atom
        {otp, _} -> true
    end.

vm_version() ->
    case erlang:system_info(machine) of
        "ATOM" ->
            atom;
        "BEAM" ->
            OTPRelease = erlang:system_info(otp_release),
            Version = list_to_integer(OTPRelease),
            {otp, Version}
    end.
