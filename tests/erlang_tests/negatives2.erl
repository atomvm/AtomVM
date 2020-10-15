-module(negatives2).

-export([start/0, f/2]).

start() ->
    -f(20, 10) * 5.

f(A, B) ->
    (A - B) * 10.
