-module(negatives).
-export([start/0]).

start() ->
    op(31420, 8192, -1).

op(A, B, C) ->
    (id(A) + id(B) * 3) * C.

id(0) ->
    0;
id(I) ->
    I.
