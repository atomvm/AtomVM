-module(compact15bitsinteger).

-export([start/0]).

start() ->
    op(31420, 8192, 48).

op(A, B, C) ->
    (id(A) + id(B) * 3) * (C - 20).

id(0) ->
    0;
id(I) ->
    I.
