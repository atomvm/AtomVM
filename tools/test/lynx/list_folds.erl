% Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(list_folds).
-export([observe/4]).

observe(Operation, Shape, Items, Mode) ->
    Input =
        case Shape of
            proper -> Items;
            improper -> Items ++ tail;
            non_list -> not_a_list
        end,
    Captured = {captured, Items, <<5:3>>},
    Seed = {seed, 7, <<3:2>>},
    put(list_fold_calls, []),
    try
        Callback =
            case Mode of
                non_function -> not_fun;
                wrong_arity -> fun(_) -> wrong end;
                _ -> fun(X, Acc) -> step(Operation, Mode, X, Acc, Captured) end
            end,
        Outcome =
            try apply(lists, Operation, [Callback, Seed, Input]) of
                Result -> {ok, Result}
            catch
                error:{badarity, {BadFun, Args}} ->
                    {exception, error, {badarity, BadFun =:= Callback, Args}};
                Class:Reason ->
                    {exception, Class, Reason}
            end,
        true = erlang:garbage_collect(),
        {Outcome, lists:reverse(get(list_fold_calls)), Input, Captured}
    after
        erase(list_fold_calls)
    end.

step(Operation, Mode, X, Acc, Captured) ->
    Calls = get(list_fold_calls),
    N = length(Calls) + 1,
    put(list_fold_calls, [{X, Acc, Captured} | Calls]),
    true = erlang:garbage_collect(),
    case Mode of
        {fail, Class, N} ->
            case Class of
                error -> error({callback_failed, X, <<5:3>>});
                throw -> throw({callback_failed, X, <<5:3>>});
                exit -> exit({callback_failed, X, <<5:3>>})
            end;
        {bad_return, Bad, N} ->
            Bad;
        _ ->
            Next =
                case {Mode, N} of
                    {replace_acc, 2} -> {replacement, <<5:3>>};
                    _ -> {X, Acc}
                end,
            case Operation of
                foldl -> Next;
                foldr -> Next;
                _ -> {{mapped, X, Acc, Captured}, Next}
            end
    end.
