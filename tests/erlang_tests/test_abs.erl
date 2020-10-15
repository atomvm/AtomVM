-module(test_abs).

-export([start/0, i/2, f/2, g/2]).

start() ->
    i([1, 2, 3], a) + i([1, 2], b) + i([], a).

i(L, a) ->
    abs(f(L, 0));
i(L, b) ->
    abs(g(L, 0)).

f([], Acc) ->
    -Acc;
f([_H | T], Acc) ->
    f(T, Acc + 1).

g([], Acc) ->
    Acc;
g([_H | T], Acc) ->
    f(T, Acc + 1).
