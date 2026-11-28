% Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(list_keys).
-export([observe/6]).

observe(Operation, Key, Position, Shape, Items, Replacement) ->
    Input =
        case Shape of
            proper -> Items;
            improper -> Items ++ tail;
            non_list -> {not_a_list, Items}
        end,
    Captured = {captured, Key, Position, Input, Replacement, <<5:3>>, 1 bsl 100},
    true = erlang:garbage_collect(),
    Outcome =
        try run(Operation, Key, Position, Input, Replacement) of
            Result -> {ok, Result}
        catch
            Class:Reason -> {exception, Class, Reason}
        end,
    true = erlang:garbage_collect(),
    {Outcome, Input, Captured}.

run(keydelete, Key, Position, Input, _Replacement) ->
    lists:keydelete(Key, Position, Input);
run(keyreplace, Key, Position, Input, Replacement) ->
    lists:keyreplace(Key, Position, Input, Replacement);
run(keystore, Key, Position, Input, Replacement) ->
    lists:keystore(Key, Position, Input, Replacement);
run(keytake, Key, Position, Input, _Replacement) ->
    lists:keytake(Key, Position, Input);
run(keysort, _Key, Position, Input, _Replacement) ->
    lists:keysort(Position, Input);
run(ukeysort, _Key, Position, Input, _Replacement) ->
    lists:ukeysort(Position, Input).
