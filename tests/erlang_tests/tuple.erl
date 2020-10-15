-module(tuple).

-export([start/0, maketuple/3, sumtuple/1]).

start() -> sumtuple(maketuple(1, 2, 3)).

maketuple(A, B, C) -> {A, B, C}.

sumtuple({A, B, C}) -> A + B + C.
