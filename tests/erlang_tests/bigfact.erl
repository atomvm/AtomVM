-module(bigfact).

-export([start/0, factorial/1]).

start() ->
    bigfact:factorial(20) div bigfact:factorial(15).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).
