-module(floatadd).

-export([start/0]).

start() ->
    to_int(add(id(2.8), id(-0.8))).

add(A, B) ->
    id(A) + id(B).

to_int(A) ->
    round(A).

id(I) ->
    I.
