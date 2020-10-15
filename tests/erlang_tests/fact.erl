-module(fact).

-export([start/0]).

start() ->
    factorial(5).

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).
