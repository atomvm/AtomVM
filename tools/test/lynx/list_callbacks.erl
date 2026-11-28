% Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(list_callbacks).
-export([observe/4]).

observe(Operation, Shape, Items, Mode) ->
    Input =
        case Shape of
            proper -> Items;
            improper -> Items ++ tail;
            non_list -> not_a_list
        end,
    Captured = {captured, Items, <<5:3>>},
    put(list_callback_calls, []),
    try
        Callback =
            case Mode of
                non_function -> not_fun;
                wrong_arity -> fun(_, _) -> wrong end;
                _ -> fun(X) -> step(Operation, Mode, X, Captured) end
            end,
        Outcome =
            try apply(lists, Operation, [Callback, Input]) of
                Result -> {ok, Result}
            catch
                error:{badarity, {BadFun, Args}} ->
                    {exception, error, {badarity, BadFun =:= Callback, Args}};
                Class:Reason ->
                    {exception, Class, Reason}
            end,
        true = erlang:garbage_collect(),
        {Outcome, lists:reverse(get(list_callback_calls)), Input, Captured}
    after
        erase(list_callback_calls)
    end.

step(Operation, Mode, X, Captured) ->
    Calls = get(list_callback_calls),
    N = length(Calls) + 1,
    put(list_callback_calls, [{X, N, Captured} | Calls]),
    true = erlang:garbage_collect(),
    case Mode of
        {fail, Class, N} ->
            fail(Class, X);
        {return, Value, N} ->
            Value;
        {return_then_fail, _Value, _At, Class, N} ->
            fail(Class, X);
        {return_then_fail, Value, N, _Class, _At} ->
            Value;
        {improper_return, N} ->
            [X | returned_tail];
        {stop, At} when N > At ->
            error({unnecessary_callback, X});
        {stop, At} ->
            case Operation of
                search -> N =:= At;
                dropwhile -> N =/= At
            end;
        all_true ->
            true;
        all_false ->
            false;
        exact ->
            case Operation of
                dropwhile -> X =/= 1;
                _ -> X =:= 1
            end;
        _ ->
            Mapped = {mapped, N, X, Captured},
            case Operation of
                map ->
                    Mapped;
                foreach ->
                    #{ignored => Mapped};
                flatmap ->
                    case N rem 3 of
                        1 -> [];
                        2 -> [Mapped];
                        0 -> [Mapped, {extra, X}]
                    end;
                filter ->
                    N rem 3 =/= 2;
                filtermap ->
                    case N rem 3 of
                        1 -> false;
                        2 -> {true, Mapped};
                        0 -> true
                    end;
                search ->
                    false;
                dropwhile ->
                    true
            end
    end.

fail(error, X) -> error({callback_failed, X, <<5:3>>});
fail(throw, X) -> throw({callback_failed, X, <<5:3>>});
fail(exit, X) -> exit({callback_failed, X, <<5:3>>}).
