-module (booleans_test).
-export([start/0, f/5, one_if_true/1]).

start() ->
    one_if_true(f(false, false, false, false, true)) +
    one_if_true(f(true, false, true, true, true)) +
    one_if_true(f(true, false, true, true, true)) +
    one_if_true(f(true, false, true, true, false)) +
    one_if_true(f(false, false, false, false, false)) +
    one_if_true(f(false, false, false, false, true)).

f(A, B, C, D, E) ->
    (A and B) or (C and D) or not E.

one_if_true(true) ->
    1;
one_if_true(false) ->
    0.
