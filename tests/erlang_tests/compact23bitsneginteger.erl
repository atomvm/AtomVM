-module(compact23bitsneginteger).

-export([start/0]).

start() ->
    op(-5946590, 8192, 28).

op(A, B, C) ->
    (id(A) + id(B) * 3) * (C - 20).

id(0) ->
    0;
id(I) ->
    I.
