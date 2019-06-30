-module(boxedmul).

-export([start/0, pow/2, mulM1/1, neg/1, id/1]).

start() ->
    (mulM1(pow(-2, 31)) - 1) rem (mulM1(pow(-2, 27)) - 1).

pow(N, 0) when is_number(N) ->
    1;
pow(N, M) ->
    N * pow(N, M - 1).

mulM1(M) ->
    M * neg(1).

neg(N) ->
    -id(N).

id(I) when not is_tuple(I) ->
    I.
