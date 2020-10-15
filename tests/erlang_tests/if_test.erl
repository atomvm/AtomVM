-module(if_test).

-export([start/0, f/2, g/1]).

start() ->
    5 = f(5, mul) - g(6).

f(A, Op) ->
    if
        (Op == mul) and (A > 0) -> A * 2;
        (Op == sub) and (A > 0) -> A - 1;
        A == 0 -> 1
    end.

g(B) ->
    B - 1.
