-module(test_list_to_integer).
-export([start/0, sum_integers/2, append_0/1]).

start() ->
    sum_integers(append_0("10"), "-1").

append_0(L) ->
    L ++ "0".

sum_integers(A, B) ->
    list_to_integer(A) + list_to_integer(B).
