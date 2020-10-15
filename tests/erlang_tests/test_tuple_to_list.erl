-module(test_tuple_to_list).

-export([start/0, f/2, g/1]).

start() ->
    g(tuple_to_list(f({1, 2, 3, 4}, 10))).

f({A, B, C, D}, X) ->
    {A * X, B * X, C * X, D * X}.

g(L) ->
    g(L, 1, 0).

g([], _I, Acc) ->
    Acc;
g([H | T], I, Acc) ->
    g(T, I + 1, H * I + Acc).
