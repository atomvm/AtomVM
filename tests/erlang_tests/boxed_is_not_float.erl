-module(boxed_is_not_float).

-export([start/0, pow/2, test/1]).

start() ->
    Res = (pow(-2, 63) + 1) * (pow(2, 1) - 3),
    test(Res).

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
