-module(large_int_literal).

-export([start/0, factorial/1]).

start() ->
    (16#7FCAFEBABE02ABCD bxor factorial(10)) rem 7919.

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).
