-module(add).

-export([start/0]).

start() ->
    add(8, 9).

add(A, B) ->
    id(A) + id(B).

id(I) ->
    I.
