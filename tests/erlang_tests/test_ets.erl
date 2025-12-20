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
    ok = isolated(fun test_ets_new/0),
    ok = isolated(fun test_permissions/0),
    ok = isolated(fun test_keys/0),
    ok = isolated(fun test_keypos/0),
    ok = isolated(fun test_insert/0),
    ok = isolated(fun test_delete/0),
    ok = isolated(fun test_lookup_element/0),
    ok = isolated(fun test_update_counter/0),
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

    % Unimplemented
    ets:new(type_test, [ordered_set]),
    ets:new(type_test, [bag]),
    ets:new(type_test, [duplicate_bag]),
    ets:new(heir_test, [{heir, self(), []}]),
    ets:new(heir_test, [{heir, none}]),
    ets:new(write_conc_test, [{write_concurrency, true}]),
    ets:new(read_conc_test, [{read_concurrency, true}]),
    case otp_version() of
        OTP when OTP >= 23 -> ets:new(decent_counters_test, [{decentralized_counters, true}]);
        _ -> ok
    end,
    ets:new(compressed_test, [compressed]),
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

    TErr = ets:new(test, []),
    true = ets:insert(TErr, {key, 0, not_number}),
    true = ets:insert(TErr, {not_number, ok}),
    assert_badarg(fun() -> ets:update_counter(TErr, none, 10) end),
    assert_badarg(fun() -> ets:update_counter(TErr, not_number, 10) end),
    assert_badarg(fun() -> ets:update_counter(TErr, not_number, {1, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, not_number, {1, 10, 100, 0}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {0, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {-1, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {1, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {3, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, not_number) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {not_number, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {2, not_number}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {not_number, 10, 100, 0}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {2, not_number, 100, 0}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {2, 10, not_number, 0}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {2, 10, 100, not_number}) end),
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
    {Pid, MRef} = spawn_opt(
        fun() ->
            Ret = Fun(),
            Self ! {ok, Ref, Ret}
        end,
        [monitor]
    ),
    receive
        {ok, Ref, Ret} ->
            normal =
                receive
                    {'DOWN', MRef, process, Pid, Reason} -> Reason
                end,
            Ret;
        {'DOWN', MRef, process, Pid, Reason} ->
            {error, {process_died, Reason}}
    after 500 ->
        demonitor(MRef, [flush]),
        exit(Pid, kill),
        {error, timeout}
    end.

assert_badarg(Fun) ->
    try
        Fun(),
        erlang:error(no_throw)
    catch
        error:badarg ->
            ok;
        OtherClass:OtherError ->
            erlang:error({OtherClass, OtherError})
    end.

supports_v4_port_encoding() ->
    case erlang:system_info(machine) of
        "ATOM" ->
            % small utf8 atom
            true;
        "BEAM" ->
            OTP = otp_version(),
            if
                OTP < 24 -> false;
                % v4 is supported but not the default
                OTP < 26 -> true;
                % small utf8 atom
                true -> true
            end
    end.

otp_version() ->
    OTPRelease = erlang:system_info(otp_release),
    list_to_integer(OTPRelease).
