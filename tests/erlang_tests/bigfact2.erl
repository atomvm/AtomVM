-module(bigfact2).

-export([start/0, factorial/1]).

start() ->
    abs(-factorial(20)) rem -(factorial(12) + 1).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).
