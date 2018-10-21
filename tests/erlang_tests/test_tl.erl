-module(test_tl).
-export([start/0, f/2, g/2, h/1]).

start() ->
    h(g(b, f(3, 1))).

f(0, _V) ->
    [];

f(1, V) ->
    [V];

f(2, V) ->
    [V, V + 1];

f(3, V) ->
    [V, V + 1, V + 2].

g(a, _L) ->
    0;

g(b, L) ->
    tl(L).

h([A, B]) ->
    A + B;

h(_) ->
    20.
