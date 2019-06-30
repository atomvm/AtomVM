-module(bigfact3).

-export([start/0, factorial/1]).

start() ->
    (factorial(20) - factorial(19)) rem (factorial(10) + 1).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).
