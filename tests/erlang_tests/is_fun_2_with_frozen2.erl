-module(is_fun_2_with_frozen2).

-export([start/0, f/2, g/1, h/3, factorial/1, id/1]).

start() ->
    factorial(g(id(f(factorial(1), id(2))))).

f(A, B) ->
    C = factorial(A) + factorial(B),
    id(fun(X) -> h(A, C, X) end).

id(X) ->
    X.

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N-1).

g(X) when is_function(X, 2) ->
    -1;
g(X) when is_function(X, 1) ->
    id(factorial(id(3))) - factorial(2);
g(X) when is_function(X) ->
    1;
g(_X) ->
    0.

h(A, B, C) when is_integer(A) and is_integer(B) and is_integer(C) ->
    A + B div C;
h(_A, _B, _C) ->
    0.
