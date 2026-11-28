% Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(queue_callbacks).
-export([observe/4]).

%% Build equivalent queues through public APIs, without comparing their layouts.
observe(Operation, Layout, Items, Mode) ->
    Queue =
        case Layout of
            balanced -> queue:from_list(Items);
            rear -> lists:foldl(fun queue:in/2, queue:new(), Items);
            front -> lists:foldr(fun queue:in_r/2, queue:new(), Items)
        end,
    put(queue_calls, []),
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
                    fold -> {ok, queue:fold(Callback, {seed, <<5:3>>}, Queue)};
                    any -> {ok, queue:any(Callback, Queue)};
                    all -> {ok, queue:all(Callback, Queue)};
                    _ -> {ok, queue:to_list(apply(queue, Operation, [Callback, Queue]))}
                end
            catch
                Class:Reason -> {exception, Class, Reason}
            end,
        Calls = lists:reverse(get(queue_calls)),
        true = erlang:garbage_collect(),
        Trace =
            case Operation of
                Op when Op =:= any; Op =:= all ->
                    %% Neither API promises logical queue visitation order. Keep raw
                    %% calls for independent checking; compare counts and membership.
                    Raw = {Operation, Layout, Items, Mode, Outcome, Calls},
                    io:format("LYNX_QUEUE_TRACE ~w~n", [binary_to_list(term_to_binary(Raw))]),
                    {length(Calls), (Calls -- Items) =:= []};
                _ ->
                    Calls
            end,
        {Outcome, Trace, queue:to_list(Queue)}
    after
        erase(queue_calls)
    end.

callback_value(Operation, Mode, X, Acc) ->
    Calls = get(queue_calls),
    N = length(Calls) + 1,
    Entry =
        case Operation of
            fold -> {X, Acc};
            _ -> X
        end,
    put(queue_calls, [Entry | Calls]),
    true = erlang:garbage_collect(),
    case Mode of
        {fail, Class, N} ->
            case Class of
                throw -> throw({callback_failed, <<5:3>>});
                error -> error({callback_failed, <<5:3>>});
                exit -> exit({callback_failed, <<5:3>>})
            end;
        invalid_return when N =:= 2 -> not_boolean;
        short when N =:= 1 -> Operation =:= any;
        short ->
            error(unnecessary_callback);
        all_true ->
            true;
        all_false ->
            false;
        transform ->
            case {Operation, X} of
                {_, {drop, _}} ->
                    false;
                {filter, {nil, _}} ->
                    [];
                {filter, {one, V}} ->
                    [{wrapped, V}];
                {filter, {many, V}} ->
                    [{left, V}, {right, V}];
                {filtermap, {Tag, _}} when Tag =:= nil; Tag =:= one; Tag =:= many ->
                    {true, {mapped, X}};
                _ ->
                    true
            end;
        _ ->
            case {Operation, Mode} of
                {fold, _} -> {X, Acc};
                {Op, normal} when Op =:= delete_with; Op =:= delete_with_r -> X =:= 1;
                {filter, _} -> true;
                {filtermap, _} -> true;
                {all, _} -> true;
                _ -> false
            end
    end.
