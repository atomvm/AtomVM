-module(test_funs1).

-export([start/0, plus5/1, f/2]).

start() ->
    C = f("1", "2"),
    plus5(fun() -> C end) +
    plus5(fun() -> 0 end) * 100.

plus5(F) ->
    F() + 5.

f(A, B) ->
    list_to_integer(A ++ B).
