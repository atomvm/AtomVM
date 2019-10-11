-module(floatmul).

-export([start/0]).

start() ->
    to_int(id(mul(id(100), id(0.5)))).

mul(A, B) ->
    id(A) * id(B).

to_int(A) ->
    round(A).

id(I) when is_float(I) ->
    I;
id(I) when is_integer(I) ->
    -I.
