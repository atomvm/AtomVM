% Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(set_callbacks).
-export([observe/4]).

%% Construction histories are public operations, not assumptions about layout.
observe(Operation, History, Items, Mode) ->
    Set =
        case History of
            from_list ->
                sets:from_list(Items, [{version, 2}]);
            added ->
                lists:foldl(fun sets:add_element/2, sets:new([{version, 2}]), Items);
            reinserted ->
                Original = sets:from_list(Items, [{version, 2}]),
                Empty = lists:foldl(fun sets:del_element/2, Original, Items),
                lists:foldl(fun sets:add_element/2, Empty, lists:reverse(Items))
        end,
    put(set_calls, []),
    try
        Callback =
            case {Operation, Mode} of
                {_, non_function} -> not_fun;
                {fold, wrong_arity} -> fun(_) -> wrong end;
                {_, wrong_arity} -> fun(_, _) -> wrong end;
                {fold, _} -> fun(X, Acc) -> callback_value(Operation, Mode, X, Acc) end;
                _ -> fun(X) -> callback_value(Operation, Mode, X, unused) end
            end,
        Outcome =
            try
                case Operation of
                    fold -> {ok, sets:fold(Callback, {{seed, <<5:3>>}, 7, #{}}, Set)};
                    _ -> {ok, differential:set_elements(apply(sets, Operation, [Callback, Set]))}
                end
            catch
                Class:Reason -> {exception, Class, Reason}
            end,
        Calls = lists:reverse(get(set_calls)),
        Visits =
            case Operation of
                fold -> [X || {X, _} <- Calls];
                _ -> Calls
            end,
        Valid =
            length(Visits) =:= map_size(maps:from_keys(Visits, present)) andalso
                lists:all(fun(X) -> sets:is_element(X, Set) end, Visits),
        %% Never compare unspecified traversal order or the exception prefix.
        %% Preserve raw visits so the independent checker can check each VM.
        Raw = {Operation, History, Items, Mode, Outcome, Calls},
        io:format("LYNX_SET_TRACE ~w~n", [binary_to_list(term_to_binary(Raw))]),
        true = erlang:garbage_collect(),
        {Outcome, {length(Calls), Valid}, differential:set_elements(Set)}
    after
        erase(set_calls)
    end.

callback_value(Operation, Mode, X, Acc) ->
    Calls = get(set_calls),
    N = length(Calls) + 1,
    Entry =
        case Operation of
            fold -> {X, Acc};
            _ -> X
        end,
    put(set_calls, [Entry | Calls]),
    true = erlang:garbage_collect(),
    case Mode of
        {fail, Class, N} ->
            case Class of
                throw -> throw({callback_failed, <<5:3>>});
                error -> error({callback_failed, <<5:3>>});
                exit -> exit({callback_failed, <<5:3>>})
            end;
        invalid_return when N =:= 2 ->
            not_boolean;
        _ ->
            case {Operation, Mode, X} of
                {fold, _, _} ->
                    {Seed, Count, Seen} = Acc,
                    {Seed, Count + 1, Seen#{X => present}};
                {map, identity, _} ->
                    X;
                {map, constant, _} ->
                    collision;
                {map, _, {merge, _}} ->
                    collision;
                {map, _, Value} when Value =:= 1; Value =:= 1.0 -> Value;
                {map, _, _} ->
                    {wrapped, X};
                {_, all_false, _} ->
                    false;
                {_, all_true, _} ->
                    true;
                {filter, _, _} ->
                    X =/= 1;
                {filtermap, _, {drop, _}} ->
                    false;
                {filtermap, _, {keep, _}} ->
                    true;
                {filtermap, _, {merge, _}} ->
                    {true, collision};
                {filtermap, _, _} ->
                    {true, {wrapped, X}}
            end
    end.
