-module(test_gt_and_le).

-export([start/0, f/4, h/4, i/4, j/4]).

start() ->
    g(f(5, 5, -1, 1), 1) +
        g(h(0, -1, -2, -3), 2) +
        g(i(-2, -2, 10, 0), 4) +
        g(j(-2, -2, -10, 5), 8) +
        g(not f(5, 3, -1, -10), 16) +
        g(not h(1, 2, -1, 5), 32) +
        g(not i(1, 2, -1, 5), 64) +
        g(not j(5, 3, -1, -10), 128).

f(A, B, C, D) ->
    (A < B) xor (C < D).

h(A, B, C, D) ->
    (A > B) and (B > C) and (C > D).

i(A, B, C, D) ->
    (A >= B) and (C >= D).

j(A, B, C, D) ->
    (A =< B) and (C =< D).

g(true, N) ->
    N;
g(false, _N) ->
    0.
