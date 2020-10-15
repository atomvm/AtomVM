-module(test_list_to_integer).

-export([start/0, sum_integers/2, append_0/1]).

start() ->
    sum_integers(append_0("10"), "-1") + safe_list_to_integer("--") - safe_list_to_integer(nan) +
        safe_list_to_integer("+10") - 10 + safe_list_to_integer("-") - 5 + safe_list_to_integer("+") -
        5 +
        safe_list_to_integer("") - 5.

append_0(L) ->
    L ++ "0".

sum_integers(A, B) ->
    list_to_integer(A) + list_to_integer(B).

safe_list_to_integer(A) ->
    try list_to_integer(A) of
        AnyValue -> AnyValue
    catch
        error:badarg ->
            5;
        _:_ ->
            "error"
    end.
