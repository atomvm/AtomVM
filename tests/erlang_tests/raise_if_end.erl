-module(raise_if_end).

-export([do_raise/2]).

do_raise(A, B) ->
    if
        A > B -> 1;
        A < B -> 2
    end.
