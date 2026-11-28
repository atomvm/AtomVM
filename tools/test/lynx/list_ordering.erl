% Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(list_ordering).
-export([observe/5]).

observe(Operation, Direction, Left, Right, Mode) ->
    Captured = {captured, Left, Right, <<5:3>>, 1 bsl 100},
    put(ordering_capture, Captured),
    put(ordering_gc_safe, true),
    try
        Callback =
            case Mode of
                non_function -> not_fun;
                arity_one -> fun(_) -> wrong end;
                arity_three -> fun(_, _, _) -> wrong end;
                _ -> fun(A, B) -> compare(A, B, Direction, Mode, Captured) end
            end,
        Outcome =
            try run(Operation, Callback, Left, Right) of
                Result -> {ok, Result}
            catch
                error:{badarity, {BadFun, _Args}} ->
                    {exception, error, {badarity, BadFun =:= Callback}};
                Class:Reason ->
                    {exception, Class, Reason}
            end,
        true = erlang:garbage_collect(),
        {Outcome, get(ordering_gc_safe), Left, Right, Captured}
    after
        erase(ordering_capture),
        erase(ordering_gc_safe)
    end.

run(sort, Callback, Left, _Right) -> lists:sort(Callback, Left);
run(usort, Callback, Left, _Right) -> lists:usort(Callback, Left);
run(merge, Callback, Left, Right) -> lists:merge(Callback, Left, Right).

compare(A, B, Direction, Mode, Captured) ->
    true = erlang:garbage_collect(),
    {captured, Left, Right, _, _} = Captured,
    Safe =
        get(ordering_gc_safe) andalso get(ordering_capture) =:= Captured andalso
            lists:member(A, Left ++ Right) andalso lists:member(B, Left ++ Right),
    put(ordering_gc_safe, Safe),
    case Mode of
        {fail, error} ->
            error({comparator_failed, <<5:3>>});
        {fail, throw} ->
            throw({comparator_failed, <<5:3>>});
        {fail, exit} ->
            exit({comparator_failed, <<5:3>>});
        {return, Value} ->
            Value;
        normal ->
            case Direction of
                ascending -> element(1, A) =< element(1, B);
                descending -> element(1, A) >= element(1, B)
            end
    end.
