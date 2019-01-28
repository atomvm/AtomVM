-module(test_funs9).

-export([start/0, plus5/1, f/2]).

start() ->
    C = {"10", "20", "30"},
    plus5(fun() -> g(C) end) +
    plus5(fun() -> 0 end) * 100.

plus5(F) ->
    F() + 5.

f(A, B) ->
    list_to_integer(A ++ B).

g({A, B, C}) ->
    f(A, B) + f(B, C).
