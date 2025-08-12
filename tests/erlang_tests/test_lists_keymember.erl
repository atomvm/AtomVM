-module(test_lists_keymember).

-export([start/0]).

start() ->
    ok = test_keymember_with_existing_key(),
    ok = test_keymember_with_non_existing_key(),
    ok = test_keymember_with_empty_list(),
    ok = test_keymember_with_existing_key_on_different_position(),
    ok = test_keymember_with_existing_key_and_different_length_of_tuples(),
    0.

test_keymember_with_existing_key() ->
    Key = bob,
    Tuple1 = {1, alice},
    Tuple2 = {2, bob},
    Tuple3 = {3, carol},
    List = [Tuple1, Tuple2, Tuple3],
    true = lists:keymember(Key, 2, List),
    ok.

test_keymember_with_existing_key_and_different_length_of_tuples() ->
    Key = is,
    Tuple1 = {1},
    Tuple2 = {2, bob},
    Tuple3 = {3, carol, singing, tree},
    Tuple4 = {here, it, is},
    List = [Tuple1, Tuple2, Tuple3, Tuple4],
    true = lists:keymember(Key, 3, List),
    ok.

test_keymember_with_non_existing_key() ->
    Key = 4,
    Tuple1 = {1, alice},
    Tuple2 = {2, bob},
    Tuple3 = {3, carol},
    List = [Tuple1, Tuple2, Tuple3],
    false = lists:keymember(Key, 1, List),
    ok.

test_keymember_with_empty_list() ->
    Key = 3,
    List = [],
    false = lists:keymember(Key, 1, List),
    ok.

test_keymember_with_existing_key_on_different_position() ->
    Key = 1,
    Tuple1 = {1, alice},
    Tuple2 = {4, bob},
    Tuple3 = {3, carol},
    List = [Tuple1, Tuple2, Tuple3],
    false = lists:keymember(Key, 2, List),
    ok.
