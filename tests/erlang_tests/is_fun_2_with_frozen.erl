-module(is_fun_2_with_frozen).

-export([start/0, f/2, g/1, factorial/1, id/1]).

start() ->
    factorial(g(id(f(factorial(1), id(2))))).

f(A, B) ->
    C = factorial(A) + factorial(B),
    id(fun() -> id(C) end).

id(X) ->
    X.

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N - 1).

g(X) when is_function(X, 1) ->
    -1;
g(X) when is_function(X, 0) ->
    id(factorial(id(3))) - factorial(2);
g(X) when is_function(X) ->
    1;
g(_X) ->
    0.
