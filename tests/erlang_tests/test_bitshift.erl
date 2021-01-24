-module(test_bitshift).

-export([start/0, shift_left/2, shift_right/2]).

start() ->
    test_shift(64 - 3),
    0.

test_shift(M) ->
    test_shift(0, M, 1).

test_shift(N, N, _E) ->
    ok;
test_shift(N, M, E) ->
    verify_shift(E, 16#01, N),
    test_shift(N + 1, M, E * 2).

verify_shift(E, A, B) ->
    E = shift_left(A, B),
    A = shift_right(E, B).

shift_left(A, B) ->
    A bsl B.

shift_right(A, B) ->
    A bsr B.
