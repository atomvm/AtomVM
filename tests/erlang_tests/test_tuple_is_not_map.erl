-module(test_tuple_is_not_map).
-export([start/0, f/1, g/1, fact/1, id/1]).

start() ->
    id(g(id(fact(id(1))))).

fact(0) ->
    1;
fact(N) ->
    N * fact(N - 1).

id(X) when is_integer(X) ->
    X.

f(0) ->
    [];
f(1) ->
    {1};
f(_) ->
    <<"test">>.

g(X) when is_map(X) ->
    8;
g(_X) ->
    16.
