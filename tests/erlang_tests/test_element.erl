-module(test_element).

-export([start/0, f/2, g/1]).

start() ->
    f({1, hello}, g(0)) + f({1, 2, world}, g(2)) + f({4}, g(0)).

f(X, N) ->
    element(N, X).

g(0) ->
    1;
g(N) ->
    g(N - 1) * N.
