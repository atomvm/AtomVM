-module (rem_and_comp_test).
-export([start/0, rem_func/2]).

start() ->
    one_if_true(rem_func(5, 3) =:= 2) +
    one_if_true(rem_func(0, 1) =:= 0) +
    one_if_true(rem_func(0, 1) =:= 0) +
    one_if_true(rem_func(0, 1) =/= 1) +
    one_if_true(rem_func(0, 1) =/= 0) +
    one_if_true(rem_func(0, 2) =/= 0).

rem_func(A, B) ->
    A rem B.

one_if_true(true) ->
    1;
one_if_true(false) ->
    0.
