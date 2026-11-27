% Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(list_zipwith).
-export([observe/5]).

observe(LeftShape, LeftItems, RightShape, RightItems, Mode) ->
    Left = input(LeftShape, LeftItems, left_tail),
    Right = input(RightShape, RightItems, right_tail),
    Captured = {captured, Left, Right, <<5:3>>},
    put(zipwith_calls, []),
    try
        Callback =
            case Mode of
                non_function -> not_fun;
                arity_zero -> fun() -> wrong end;
                arity_one -> fun(_) -> wrong end;
                arity_three -> fun(_, _, _) -> wrong end;
                _ -> fun(A, B) -> step(A, B, Mode, Captured) end
            end,
        Outcome =
            try lists:zipwith(Callback, Left, Right) of
                Result -> {ok, Result}
            catch
                error:{badarity, {BadFun, Args}} ->
                    {exception, error, {badarity, BadFun =:= Callback, Args}};
                Class:Reason ->
                    {exception, Class, Reason}
            end,
        true = erlang:garbage_collect(),
        {Outcome, lists:reverse(get(zipwith_calls)), Left, Right, Captured}
    after
        erase(zipwith_calls)
    end.

input(proper, Items, _Tail) -> Items;
input(improper, Items, Tail) -> Items ++ Tail;
input(non_list, _Items, Tail) -> {not_a_list, Tail}.

step(A, B, Mode, Captured) ->
    Calls = get(zipwith_calls),
    N = length(Calls) + 1,
    put(zipwith_calls, [{A, B, N, Captured} | Calls]),
    true = erlang:garbage_collect(),
    case Mode of
        {fail, Class, N} -> fail(Class, N, A, B);
        {return, Value, N} -> Value;
        {return_then_fail, _Value, _At, Class, N} -> fail(Class, N, A, B);
        {return_then_fail, Value, N, _Class, _At} -> Value;
        {improper_return, N} -> [A | B];
        pair -> {A, B};
        _ -> {combined, N, A, B, Captured}
    end.

fail(error, N, A, B) -> error({callback_failed, N, A, B, <<5:3>>});
fail(throw, N, A, B) -> throw({callback_failed, N, A, B, <<5:3>>});
fail(exit, N, A, B) -> exit({callback_failed, N, A, B, <<5:3>>}).
