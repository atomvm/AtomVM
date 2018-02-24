-module (booleans2_test).
-export([start/0, f/5, one_if_true/1]).

start() ->
    one_if_true(f(1, 1, 2, 3, 4)) +
    one_if_true(f(1, 2, 3, 3, 4)) +
    one_if_true(f(1, 2, 3, 4, 5)) +
    one_if_true(f(1, 1, 2, 2, 6)) +
    one_if_true(f(1, 1, 3, 4, 1)).

f(A, B, C, D, E) ->
    ((A == B) xor (C == D)) and (A /= E) and (C /= E).

one_if_true(true) ->
    1;
one_if_true(false) ->
    0.
