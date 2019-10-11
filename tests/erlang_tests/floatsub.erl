-module(floatsub).

-export([start/0]).

start() ->
    to_int(sub(id(2.8), id(0.8))).

sub(A, B) ->
    id(A) - id(B).

to_int(A) ->
    round(A).

id(I) ->
    I.
