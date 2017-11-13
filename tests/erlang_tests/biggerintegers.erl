-module(biggerintegers).

-export([start/0]).

start() ->
    add(500, 50).

add(A, B) ->
    id(A) + id(B).

id(0) ->
    0;
id(I) ->
    I.
