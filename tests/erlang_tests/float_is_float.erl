-module(float_is_float).

-export([start/0, pow/2, test/1, id/1]).

start() ->
    Res = (pow(-2, 63) + id(10.0)) * id(-1.0),
    test(Res).

id(I) when is_float(I) ->
    I.

pow(_N, 0) ->
    1;
pow(N, M) ->
    N * pow(N, M - 1).

test(Val) when is_float(Val) ->
    32;
test(Val) when is_integer(Val) ->
    16;
test(_Val) ->
    8.
