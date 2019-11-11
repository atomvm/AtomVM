-module(raise_badmatch).

-export([do_raise/2]).

do_raise(A, B) ->
    A = factorial(B).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).
