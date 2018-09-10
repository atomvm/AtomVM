-module(compact27bitsinteger).

-export([start/0]).

start() ->
    op(61591023, 123456).

op(A, B) ->
    id(A) + id(B) * 2.

id(0) ->
    0;
id(I) ->
    I.
