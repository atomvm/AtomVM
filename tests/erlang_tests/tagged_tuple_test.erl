-module(tagged_tuple_test).
-export([start/0, test_func/1, call_test_func/1, some_value/1]).

start() ->
    call_test_func({test, some_value(1)}).

call_test_func({test, V}) ->
    test_func({c, V}).

some_value(A) ->
    A * 2 + 1.

test_func({a, A}) ->
    A;
test_func({b, B}) ->
    B * 2;
test_func({E_A, E}) when is_atom(E_A) ->
    E * 16;
test_func({f, F, F_B}) ->
    F - F_B;
test_func(G) when is_tuple(G) ->
    0;
test_func({A, B}) when is_integer(A) and is_integer(B) ->
    A - B.
