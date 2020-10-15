-module(test_bitwise).

-export([start/0, f/3, g/1, h/1]).

start() ->
    f(2#00010 bor g(2) bor h(2), 2#01010, 2#00001).

f(A, B, C) ->
    bnot (A band B) bxor C.

g(N) ->
    2#1 bsl N.

h(N) ->
    2#0100 bsr N.
