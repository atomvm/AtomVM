-module(floatneg).

-export([start/0, add/2, id/1, to_int/1]).

start() ->
    to_int(-add(id(2.8), id(-0.8))).

add(A, B) ->
    id(A) + id(B).

to_int(A) ->
    round(A).

id(I) ->
    I.
