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
    ok = isolated(fun test_lookup_element/0),
    ok = isolated(fun test_member/0),
    ok = isolated(fun test_insert/0),
    ok = isolated(fun test_insert_new/0),
    ok = isolated(fun test_update_element/0),
    ok = isolated(fun test_update_counter/0),
    ok = isolated(fun test_take/0),
    ok = isolated(fun test_delete/0),
    ok = isolated(fun test_delete_object/0),
    0.

test_ets_new() ->
    assert_badarg(fun() -> ets:new([isnt, atom], []) end),
    assert_badarg(fun() -> ets:new(name, not_a_list) end),

    ets:new(unnamed_test, []),

    named_test = ets:new(named_test, [named_table]),
    true = assert_operation(named_test, insert, []),
    ets:new(named_test, []),
    assert_badarg(fun() -> ets:new(named_test, [named_table]) end),
    assert_badarg(fun() -> ets:new(named_test, [{named_table, not_a_boolean}]) end),

    ets:new(keypos_test, [{keypos, 2}]),
    assert_badarg(fun() -> ets:new(keypos_test, [{keypos, 0}]) end),
    assert_badarg(fun() -> ets:new(keypos_test, [{keypos, -1}]) end),

    ets:new(type_test, [set]),
    ets:new(type_test, [bag]),
    ets:new(type_test, [duplicate_bag]),

    % Unimplemented
    ets:new(type_test, [ordered_set]),
    ets:new(heir_test, [{heir, self(), []}]),
    ets:new(heir_test, [{heir, none}]),
    ets:new(write_conc_test, [{write_concurrency, true}]),
    ets:new(read_conc_test, [{read_concurrency, true}]),
    if_otp_version(23, fun() -> ets:new(decent_counters_test, [{decentralized_counters, true}]) end),
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
    ok = assert_stored_key(T, []),
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

test_lookup_element() ->
    PosKey = 1,
    PosValue = 2,

    AssertBadArgs =
        fun(Tab) ->
            PosPastBounds = 3,
            PosZero = 0,
            PosNegative = -1,

            assert_badarg(fun() -> ets:lookup_element(Tab, key_not_exist, PosKey) end),
            assert_badarg(fun() -> ets:lookup_element(Tab, key, PosZero) end),
            assert_badarg(fun() -> ets:lookup_element(Tab, key, PosPastBounds) end),
            assert_badarg(fun() -> ets:lookup_element(Tab, key, PosNegative) end),
            % lookup_element/4 with Default since OTP 26.0
            if_otp_version(26, fun() ->
                assert_badarg(fun() -> ets:lookup_element(Tab, key, PosNegative, default) end)
            end)
        end,

    % Set
    S = new_table([{key, value}]),
    key = ets:lookup_element(S, key, PosKey),
    value = ets:lookup_element(S, key, PosValue),
    % lookup_element/4 with Default since OTP 26.0
    if_otp_version(26, fun() -> default = ets:lookup_element(S, key_not_exist, PosKey, default) end),

    % Bag
    B = new_table(bag, [{key, value}, {key, value2}]),
    [key, key] = ets:lookup_element(B, key, PosKey),
    [value, value2] = ets:lookup_element(B, key, PosValue),
    if_otp_version(26, fun() -> default = ets:lookup_element(B, key_not_exist, PosKey, default) end),

    true = ets:insert(B, {key, value3}),
    [key, key, key] = ets:lookup_element(B, key, PosKey),
    [value, value2, value3] = ets:lookup_element(B, key, PosValue),

    % Duplicate bag
    DB = new_table(duplicate_bag, [{key, value}, {key, value}]),
    [key, key] = ets:lookup_element(DB, key, PosKey),
    [value, value] = ets:lookup_element(DB, key, PosValue),
    if_otp_version(26, fun() -> default = ets:lookup_element(DB, key_not_exist, PosKey, default) end),

    true = ets:insert(DB, {key, value2}),
    [key, key, key] = ets:lookup_element(DB, key, PosKey),
    [value, value, value2] = ets:lookup_element(DB, key, PosValue),

    true = ets:insert(DB, {key2, value2}),
    [key2] = ets:lookup_element(DB, key2, PosKey),
    [value2] = ets:lookup_element(DB, key2, PosValue),

    % Badargs
    assert_badarg(fun() -> ets:lookup_element(bad_table, key, 1) end),
    AssertBadArgs(ets:new(test, [set])),
    AssertBadArgs(ets:new(test, [bag])),
    AssertBadArgs(ets:new(test, [duplicate_bag])),

    ok.

test_member() ->
    % Set
    S = new_table([{key, value}]),
    true = ets:member(S, key),
    false = ets:member(S, key_not_exist),

    % Bag
    B = new_table(bag, [{key, value}, {key, value2}]),
    true = ets:member(B, key),
    false = ets:member(B, key_not_exist),

    % Duplicate bag
    DB = new_table(duplicate_bag, [{key, value}, {key, value}]),
    true = ets:member(DB, key),
    false = ets:member(DB, key_not_exist),

    % Badargs
    assert_badarg(fun() -> ets:member(bad_table, key) end),

    ok.

test_insert() ->
    % Set
    S1 = ets:new(test, []),
    [] = ets:lookup(S1, key),

    % {Key, Value}
    true = ets:insert(S1, {key, value}),
    [{key, value}] = ets:lookup(S1, key),
    true = ets:insert(S1, {key, new_value}),
    [{key, new_value}] = ets:lookup(S1, key),

    % [{Key, Value}, ...]
    S2 = ets:new(test, []),
    true = ets:insert(S2, []),
    true = ets:insert(S2, [{key, value}, {key2, value2}]),
    true = ets:insert(S2, [{key2, new_value2}, {key3, value3}]),
    [{key, value}] = ets:lookup(S2, key),
    [{key2, new_value2}] = ets:lookup(S2, key2),
    [{key3, value3}] = ets:lookup(S2, key3),

    % Bag
    B1 = ets:new(test, [bag]),
    [] = ets:lookup(B1, key),

    % {Key, Value}
    true = ets:insert(B1, {key, value}),
    [{key, value}] = ets:lookup(B1, key),
    true = ets:insert(B1, {key, new_value}),
    [{key, value}, {key, new_value}] = ets:lookup(B1, key),
    true = ets:insert(B1, {key, new_value}),
    [{key, value}, {key, new_value}] = ets:lookup(B1, key),

    % [{Key, Value}, ...]
    B2 = ets:new(test, [bag]),
    true = ets:insert(B2, []),
    true = ets:insert(B2, [{key, value}, {key2, value2}]),
    true = ets:insert(B2, [{key2, new_value2}, {key3, value3}]),
    true = ets:insert(B2, [{key2, new_value2}, {key3, new_value3}]),
    [{key, value}] = ets:lookup(B2, key),
    [{key2, value2}, {key2, new_value2}] = ets:lookup(B2, key2),
    [{key3, value3}, {key3, new_value3}] = ets:lookup(B2, key3),

    % Duplicate bag
    DB1 = ets:new(test, [duplicate_bag]),
    [] = ets:lookup(DB1, key),

    % {Key, Value}
    true = ets:insert(DB1, {key, value}),
    [{key, value}] = ets:lookup(DB1, key),
    true = ets:insert(DB1, {key, new_value}),
    [{key, value}, {key, new_value}] = ets:lookup(DB1, key),
    true = ets:insert(DB1, {key, new_value}),
    [{key, value}, {key, new_value}, {key, new_value}] = ets:lookup(DB1, key),

    % [{Key, Value}, ...]
    DB2 = ets:new(test, [duplicate_bag]),
    true = ets:insert(DB2, []),
    true = ets:insert(DB2, [{key, value}, {key2, value2}]),
    true = ets:insert(DB2, [{key2, new_value2}, {key3, value3}]),
    true = ets:insert(DB2, [{key2, new_value2}, {key3, new_value3}]),
    [{key, value}] = ets:lookup(DB2, key),
    [{key2, value2}, {key2, new_value2}, {key2, new_value2}] = ets:lookup(DB2, key2),
    [{key3, value3}, {key3, new_value3}] = ets:lookup(DB2, key3),

    % Badargs
    assert_insert_badargs(ets:new(test, []), fun ets:insert/2),
    assert_insert_badargs(ets:new(test, [bag]), fun ets:insert/2),
    assert_insert_badargs(ets:new(test, [duplicate_bag]), fun ets:insert/2),

    ok.

test_insert_new() ->
    % Set
    S1 = ets:new(test, []),
    [] = ets:lookup(S1, key),

    % {Key, Value}
    true = ets:insert_new(S1, {key, value}),
    [{key, value}] = ets:lookup(S1, key),
    false = ets:insert_new(S1, {key, new_value}),
    [{key, value}] = ets:lookup(S1, key),

    % [{Key, Value}, ...]
    S2 = ets:new(test, []),
    true = ets:insert_new(S2, []),
    true = ets:insert_new(S2, [{key, value}, {key2, value2}]),
    false = ets:insert_new(S2, [{key2, new_value2}, {key3, value3}]),
    true = ets:insert_new(S2, [{key4, value4}, {key3, value3}, {key4, new_value4}]),
    [{key, value}] = ets:lookup(S2, key),
    [{key2, value2}] = ets:lookup(S2, key2),
    [{key3, value3}] = ets:lookup(S2, key3),
    [{key4, new_value4}] = ets:lookup(S2, key4),

    % Regression: existing keys must not bypass validation of the rest of the list
    S3 = new_table([{key, value}]),
    assert_badarg(fun() -> ets:insert_new(S3, [{key, new_value}, not_a_tuple]) end),
    [{key, value}] = ets:lookup(S3, key),

    % Bag
    B1 = ets:new(test, [bag]),
    [] = ets:lookup(B1, key),

    % {Key, Value}
    true = ets:insert_new(B1, {key, value}),
    [{key, value}] = ets:lookup(B1, key),
    false = ets:insert_new(B1, {key, new_value}),
    [{key, value}] = ets:lookup(B1, key),

    % [{Key, Value}, ...]
    B2 = ets:new(test, [bag]),
    true = ets:insert_new(B2, []),
    true = ets:insert_new(B2, [{key, value}, {key2, value2}]),
    false = ets:insert_new(B2, [{key2, new_value2}, {key3, value3}]),
    true = ets:insert_new(B2, [{key4, value4}, {key3, value3}, {key4, new_value4}]),
    [{key, value}] = ets:lookup(B2, key),
    [{key2, value2}] = ets:lookup(B2, key2),
    [{key3, value3}] = ets:lookup(B2, key3),
    [{key4, value4}, {key4, new_value4}] = ets:lookup(B2, key4),

    % Regression: existing keys must not bypass validation of the rest of the list
    B3 = new_table([{key, value}]),
    assert_badarg(fun() -> ets:insert_new(B3, [{key, new_value}, not_a_tuple]) end),
    [{key, value}] = ets:lookup(B3, key),

    % Duplicate bag
    DB1 = ets:new(test, [duplicate_bag]),
    [] = ets:lookup(DB1, key),

    % {Key, Value}
    true = ets:insert_new(DB1, {key, value}),
    [{key, value}] = ets:lookup(DB1, key),
    false = ets:insert_new(DB1, {key, new_value}),
    [{key, value}] = ets:lookup(DB1, key),

    % [{Key, Value}, ...]
    DB2 = ets:new(test, [duplicate_bag]),
    true = ets:insert_new(DB2, []),
    true = ets:insert_new(DB2, [{key, value}, {key2, value2}]),
    false = ets:insert_new(DB2, [{key2, new_value2}, {key3, value3}]),
    true = ets:insert_new(DB2, [{key4, value4}, {key3, value3}, {key4, value4}]),
    [{key, value}] = ets:lookup(DB2, key),
    [{key2, value2}] = ets:lookup(DB2, key2),
    [{key3, value3}] = ets:lookup(DB2, key3),
    [{key4, value4}, {key4, value4}] = ets:lookup(DB2, key4),

    % Regression: existing keys must not bypass validation of the rest of the list
    DB3 = new_table([{key, value}]),
    assert_badarg(fun() -> ets:insert_new(DB3, [{key, new_value}, not_a_tuple]) end),
    [{key, value}] = ets:lookup(DB3, key),

    % Badargs
    assert_insert_badargs(ets:new(test, []), fun ets:insert_new/2),
    assert_insert_badargs(ets:new(test, [bag]), fun ets:insert_new/2),
    assert_insert_badargs(ets:new(test, [duplicate_bag]), fun ets:insert_new/2),

    ok.

test_update_element() ->
    % {Position, Value}
    S1 = new_table({key, value1, value2}),
    true = ets:update_element(S1, key, {2, new_value1}),
    [{key, new_value1, value2}] = ets:lookup(S1, key),

    S2 = new_table({key, value1, value2}),
    true = ets:update_element(S2, key, {3, new_value2}),
    [{key, value1, new_value2}] = ets:lookup(S2, key),

    S3 = new_table({key, value1, value2}),
    false = ets:update_element(S3, key_not_exist, {2, new_value1}),
    [{key, value1, value2}] = ets:lookup(S3, key),

    % [{Position, Value}, ...]
    S4 = new_table({key, value1, value2}),
    true = ets:update_element(S4, key, [{3, new_value2}, {2, new_value1}, {3, new_last_value2}]),
    [{key, new_value1, new_last_value2}] = ets:lookup(S4, key),

    % Default object (since OTP 27.0)
    if_otp_version(27, fun() ->
        S5 = new_table({key, value1, value2}),
        true = ets:update_element(S5, key_not_exist, {2, new_value1}, {key, value1, value2}),
        [{key_not_exist, new_value1, value2}] = ets:lookup(S5, key_not_exist),

        S6 = new_table({key, value1, value2}),
        true = ets:update_element(
            S6,
            key_not_exist,
            [{2, new_value1}, {3, new_value2}, {3, new_last_value2}],
            {key, value1, value2}
        ),
        [{key_not_exist, new_value1, new_last_value2}] = ets:lookup(S6, key_not_exist)
    end),

    % Badargs
    TErr = new_table(3, [{value1, value2, key}]),
    OkDefault = {value, value, value},

    % The table type is not set
    TErrBag = ets:new(test, [bag]),
    TErrDuplBag = ets:new(test, [duplicate_bag]),
    assert_badarg(fun() -> ets:update_element(TErrBag, key, {1, value}) end),
    assert_badarg(fun() -> ets:update_element(TErrDuplBag, key, {1, value}) end),
    assert_badarg(fun() -> ets:update_element(bad_table, key, {2, value}) end),

    % Pos < 1
    assert_badarg(fun() -> ets:update_element(TErr, key, {-1, pos_neg}) end),
    assert_badarg(fun() -> ets:update_element(TErr, key, {0, pos_zero}) end),
    assert_badarg(fun() -> ets:update_element(TErr, key, [{1, pos_ok}, {0, pos_zero}]) end),

    % Pos = KeyPos
    assert_badarg(fun() -> ets:update_element(TErr, key, {3, pos_key}) end),
    assert_badarg(fun() -> ets:update_element(TErr, key, [{1, pos_ok}, {3, pos_key}]) end),

    % Pos > TupleArity
    assert_badarg(fun() -> ets:update_element(TErr, key, {4, pos_past}) end),
    assert_badarg(fun() -> ets:update_element(TErr, key, [{1, pos_ok}, {4, pos_past}]) end),

    % 4-arg update_element/4 badargs (since OTP 27.0)
    if_otp_version(27, fun() ->
        assert_badarg(fun() ->
            ets:update_element(TErr, key_not_exist, [{1, pos_ok}, {0, pos_zero}], OkDefault)
        end),
        assert_badarg(fun() ->
            ets:update_element(TErr, key_not_exist, [{1, pos_ok}, {3, pos_key}], OkDefault)
        end),
        assert_badarg(fun() ->
            ets:update_element(TErr, key_not_exist, [{1, pos_ok}, {4, pos_past}], OkDefault)
        end)
    end),

    % Default object arity < KeyPos
    % NOTE: This fails on OTP, see https://github.com/erlang/otp/issues/10603
    if_atomvm(fun() ->
        assert_badarg(fun() -> ets:update_element(TErr, key_not_exist, {1, pos_ok}, {value}) end),
        assert_badarg(fun() ->
            ets:update_element(TErr, key_not_exist, [{1, pos_ok}, {2, pos_ok}], {value, value})
        end)
    end),

    [{value1, value2, key}] = ets:lookup(TErr, key),
    [] = ets:lookup(TErr, key_not_exist),

    ok.

test_update_counter() ->
    % Increment
    S1 = new_table({key, 10, not_number, 30}),
    15 = ets:update_counter(S1, key, 5),
    [{key, 15, not_number, 30}] = ets:lookup(S1, key),

    S2 = new_table(3, {not_number, 20, key, 30}),
    -5 = ets:update_counter(S2, key, -35),
    [{not_number, 20, key, -5}] = ets:lookup(S2, key),

    % {Position, Increment}
    S3 = new_table({key, 10, 20, not_number}),
    25 = ets:update_counter(S3, key, {3, 5}),
    [{key, 10, 25, not_number}] = ets:lookup(S3, key),

    S4 = new_table({key, 10, not_number, 30}),
    0 = ets:update_counter(S4, key, {2, -10}),
    [{key, 0, not_number, 30}] = ets:lookup(S4, key),

    % []
    S5 = new_table({key, 10, not_number, 30}),
    [] = ets:update_counter(S5, key, []),
    [{key, 10, not_number, 30}] = ets:lookup(S5, key),

    % [{Position, Increment}, ...]
    S6 = new_table({key, 10, 20, not_number}),
    [0, 5, 30] = ets:update_counter(S6, key, [{2, -10}, {2, 5}, {3, 10}]),
    [{key, 5, 30, not_number}] = ets:lookup(S6, key),

    % {Position, Increment, Threshold, SetValue}
    S7 = new_table({key, not_number, 20, 30}),
    31 = ets:update_counter(S7, key, {4, 10, 39, 31}),
    [{key, not_number, 20, 31}] = ets:lookup(S7, key),

    S8 = new_table({key, 10, not_number, 30}),
    29 = ets:update_counter(S8, key, {4, -10, 21, 29}),
    [{key, 10, not_number, 29}] = ets:lookup(S8, key),

    % [{Position, Increment, Threshold, SetValue}, ...]
    S9 = new_table({key, 10, 20, not_number}),
    [20, 31, 26] = ets:update_counter(
        S9,
        key,
        [{2, 10, 20, 21}, {2, 10, 29, 31}, {3, 5, 24, 26}]
    ),
    [{key, 31, 26, not_number}] = ets:lookup(S9, key),

    % Default object
    S10 = new_table({key, 10, 20, not_number}),
    15 = ets:update_counter(S10, key_not_exist, {2, 5}, {key, 10, 20, 30}),
    [{key, 10, 20, not_number}] = ets:lookup(S10, key),
    [{key_not_exist, 15, 20, 30}] = ets:lookup(S10, key_not_exist),

    % [] with default object
    S11 = new_table({key, 10, 20, not_number}),
    [] = ets:update_counter(S11, key_not_exist2, [], {key, 10, 20, 30}),
    [{key, 10, 20, not_number}] = ets:lookup(S11, key),
    [{key_not_exist2, 10, 20, 30}] = ets:lookup(S11, key_not_exist2),

    % Badargs
    % The table type is not set
    TErrBag = ets:new(test, [bag]),
    TErrDuplBag = ets:new(test, [duplicate_bag]),
    assert_badarg(fun() -> ets:update_counter(TErrBag, key, 10) end),
    assert_badarg(fun() -> ets:update_counter(TErrDuplBag, key, 10) end),
    assert_badarg(fun() -> ets:update_counter(bad_table, key, 10) end),

    TErr = new_table(2, {0, key, not_number}),

    % Pos > KeyPos
    TErrLastKey = new_table(2, {0, key}),
    assert_badarg(fun() -> ets:update_counter(TErrLastKey, key, 10) end),

    % No object with the correct key exists and no default object was supplied
    assert_badarg(fun() -> ets:update_counter(TErr, key_not_exist, 10) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key_not_exist, {1, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key_not_exist, {1, 10, 20, 30}) end),

    % The object has the wrong arity
    % Pos > TupleArity
    assert_badarg(fun() -> ets:update_counter(TErr, key, {4, 10}) end),

    % Default object arity < KeyPos
    % NOTE: This fails on OTP, see https://github.com/erlang/otp/issues/10603
    if_atomvm(fun() ->
        assert_badarg(fun() -> ets:update_counter(TErr, key_not_exist, [{1, 10}], {10}) end)
    end),

    % Any field from the default object that is updated is not an integer
    assert_badarg(fun() ->
        ets:update_counter(
            TErr, key_not_exist, {2, 10}, {0, key, not_number}
        )
    end),
    assert_badarg(fun() ->
        ets:update_counter(
            TErr, key_not_exist, {3, 10}, {0, key, not_number}
        )
    end),

    % The element to update is not an integer
    assert_badarg(fun() -> ets:update_counter(TErr, key, 10) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {3, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, [{1, 10}, {3, 10}]) end),

    % The element to update is also the key
    assert_badarg(fun() -> ets:update_counter(TErr, key, {2, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, [{1, 10}, {2, 10}]) end),

    TErrIntKey = new_table(2, {10, 0}),
    assert_badarg(fun() -> ets:update_counter(TErrIntKey, 0, {2, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErrIntKey, 0, [{1, 10}, {2, 10}]) end),
    [{10, 0}] = ets:lookup(TErrIntKey, 0),

    Key = 10,
    assert_badarg(fun() ->
        ets:update_counter(
            TErr, key_not_exist, {2, 10}, {0, Key, not_number}
        )
    end),

    % Any of Pos, Incr, Threshold, or SetValue is not an integer
    assert_badarg(fun() -> ets:update_counter(TErr, key, not_number) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {1, not_number}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {not_number, 10}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, [{1, 10}, {1, not_number}]) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, [{1, 10}, {not_number, 10}]) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {1, 10, not_number, 30}) end),
    assert_badarg(fun() -> ets:update_counter(TErr, key, {1, 10, 20, not_number}) end),
    assert_badarg(fun() ->
        ets:update_counter(TErr, key, [{1, 10, 20, 30}, {1, 10, not_number, 30}])
    end),
    assert_badarg(fun() ->
        ets:update_counter(TErr, key, [{1, 10, 20, 30}, {1, 10, 20, not_number}])
    end),

    [{0, key, not_number}] = ets:lookup(TErr, key),

    ok.

test_take() ->
    % Set
    S1 = new_table([]),
    [] = ets:take(S1, key_not_exist),

    S2 = new_table([{key, value}, {key2, value2}]),
    [{key, value}] = ets:take(S2, key),
    [] = ets:lookup(S2, key),
    [{key2, value2}] = ets:lookup(S2, key2),

    % Bag
    B1 = new_table(bag, []),
    [] = ets:take(B1, key_not_exist),

    B2 = new_table(bag, [{key, value}, {key, value2}, {key2, value3}]),
    [{key, value}, {key, value2}] = ets:take(B2, key),
    [] = ets:lookup(B2, key),
    [{key2, value3}] = ets:lookup(B2, key2),

    % Duplicate bag
    DB1 = new_table(duplicate_bag, []),
    [] = ets:take(DB1, key_not_exist),

    DB2 = new_table(duplicate_bag, [{key, value}, {key, value}, {key2, value2}]),
    [{key, value}, {key, value}] = ets:take(DB2, key),
    [] = ets:lookup(DB2, key),
    [{key2, value2}] = ets:lookup(DB2, key2),

    % Badargs
    assert_badarg(fun() -> ets:take(bad_table, key) end),

    ok.

test_delete() ->
    % Set
    S = new_table([{key, value}, {key2, value2}]),
    true = ets:delete(S, key_not_exist),

    true = ets:delete(S, key),
    [] = ets:lookup(S, key),
    [{key2, value2}] = ets:lookup(S, key2),

    true = ets:delete(S, key2),
    [] = ets:lookup(S, key2),

    true = ets:delete(S, key2),
    true = ets:delete(S),

    % Bag
    B = new_table(bag, [{key, value}, {key, value2}, {key2, value3}]),
    true = ets:delete(B, key_not_exist),

    true = ets:delete(B, key),
    [] = ets:lookup(B, key),
    [{key2, value3}] = ets:lookup(B, key2),

    true = ets:delete(B, key2),
    [] = ets:lookup(B, key2),

    true = ets:delete(B, key2),
    true = ets:delete(B),

    % Duplicate bag
    DB = new_table(duplicate_bag, [{key, value}, {key, value}, {key2, value2}]),
    true = ets:delete(DB, key_not_exist),

    true = ets:delete(DB, key),
    [] = ets:lookup(DB, key),
    [{key2, value2}] = ets:lookup(DB, key2),

    true = ets:delete(DB, key2),
    [] = ets:lookup(DB, key2),

    true = ets:delete(DB, key2),
    true = ets:delete(DB),

    % Badargs
    TErr = new_table(2, []),
    true = ets:delete(TErr),
    assert_badarg(fun() -> ets:delete(bad_table) end),
    assert_badarg(fun() -> ets:lookup(TErr, key) end),
    assert_badarg(fun() -> ets:delete(TErr) end),

    ok.

test_delete_object() ->
    % Set
    S = new_table([{key, value}, {key2, value2}]),
    true = ets:delete_object(S, {key_not_exist, value_not_exist}),

    true = ets:delete_object(S, {key, value_not_exist}),
    [{key, value}] = ets:lookup(S, key),

    true = ets:delete_object(S, {key, value}),
    [] = ets:lookup(S, key),

    true = ets:delete_object(S, {key2, value2}),
    [] = ets:lookup(S, key2),

    % Bag
    B = new_table(bag, [{key, value}, {key, value2}, {key2, value2}]),
    true = ets:delete_object(B, {key_not_exist, value_not_exist}),

    true = ets:delete_object(B, {key, value_not_exist}),
    [{key, value}, {key, value2}] = ets:lookup(B, key),

    true = ets:delete_object(B, {key, value}),
    [{key, value2}] = ets:lookup(B, key),

    true = ets:delete_object(B, {key, value2}),
    [] = ets:lookup(B, key),

    true = ets:delete_object(B, {key2, value2}),
    [] = ets:lookup(B, key2),

    % Duplicate bag
    DB = new_table(duplicate_bag, [{key, value}, {key, value}, {key, value2}]),
    true = ets:delete_object(DB, {key_not_exist, value_not_exist}),

    true = ets:delete_object(DB, {key, value_not_exist}),
    [{key, value}, {key, value}, {key, value2}] = ets:lookup(DB, key),

    true = ets:delete_object(DB, {key, value}),
    [{key, value2}] = ets:lookup(DB, key),

    true = ets:delete_object(DB, {key, value2}),
    [] = ets:lookup(DB, key),

    % Badargs
    TErr = new_table(2, []),
    assert_badarg(fun() -> ets:delete_object(bad_table, {key, value}) end),
    assert_badarg(fun() -> ets:delete_object(TErr, not_a_tuple) end),
    assert_badarg(fun() -> ets:delete_object(TErr, [{key, value}, {key, value2}]) end),
    assert_badarg(fun() -> ets:delete_object(TErr, {}) end),
    assert_badarg(fun() -> ets:delete_object(TErr, {bad_keypos}) end),

    ok.

assert_insert_badargs(T, Insert) ->
    assert_badarg(fun() -> Insert(T, {}) end),
    assert_badarg(fun() -> Insert(T, [{}]) end),
    assert_badarg(fun() -> Insert(T, [{key, value}, not_a_tuple]) end),
    assert_badarg(fun() -> Insert(T, [{improper, true} | {list, true}]) end),
    assert_badarg(fun() -> Insert(T, not_a_tuple) end),
    assert_badarg(fun() -> Insert([not_a_ref], {key, value}) end),
    [] = ets:lookup(T, key),
    [] = ets:lookup(T, improper),
    [] = ets:lookup(T, list),
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

new_table(Tuples) ->
    new_table([], Tuples).

new_table(Type, Tuples) when is_atom(Type) ->
    new_table([Type], Tuples);
new_table(Keypos, Tuples) when is_integer(Keypos) ->
    new_table([{keypos, Keypos}], Tuples);
new_table(Opts, Tuples) when is_list(Opts) ->
    T = ets:new(test, Opts),
    true = ets:insert(T, Tuples),
    T.

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
    after 5000 ->
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
            OTP = get_otp_version(),
            if
                OTP < 24 -> false;
                % v4 is supported but not the default
                OTP < 26 -> true;
                % small utf8 atom
                true -> true
            end
    end.

get_otp_version() ->
    case erlang:system_info(machine) of
        "BEAM" -> list_to_integer(erlang:system_info(otp_release));
        _ -> atomvm
    end.

if_otp_version(MinVersion, Fun) ->
    case get_otp_version() of
        atomvm -> Fun();
        OTP when OTP >= MinVersion -> Fun();
        _ -> ok
    end.

if_atomvm(Fun) ->
    case get_otp_version() of
        atomvm -> Fun();
        _ -> ok
    end.
