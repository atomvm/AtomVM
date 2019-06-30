-module(boxedneg).

-export([start/0, pow/2]).

start() ->
    (-(pow(-2, 31)) - 1) rem (-(pow(-2, 27)) - 1).

pow(N, 0) when is_number(N) ->
    1;
pow(N, M) ->
    N * pow(N, M - 1).
