-module(test_bitwise2).

-export([start/0, f/3, g/1]).

start() ->
    f(g(18), 2#11, 2#11).

f(A, B, C) ->
    bnot (A) bor (B bxor C).

g(N) ->
    -1 bsr N.
