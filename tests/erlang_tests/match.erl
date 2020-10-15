-module(match).

-export([start/0, f/1, g/1]).

start() ->
    5 = f(5) - g(6).

f(A) ->
    A * 2.

g(B) ->
    B - 1.
