-module(raise_case_end).

-export([do_raise/1]).

do_raise(B) ->
    case factorial(B) of
        10 -> -1;
        11 -> -2;
        12 -> -3
    end.

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).
