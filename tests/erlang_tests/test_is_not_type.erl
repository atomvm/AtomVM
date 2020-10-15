-module(test_is_not_type).

-export([start/0, bool_to_integer/2, eval/6]).

start() ->
    eval({}, make_ref(), [], 10, <<"hello">>, hello).

bool_to_integer(true, _I) ->
    0;
bool_to_integer(false, I) ->
    I.

eval(A, B, C, D, E, F) ->
    bool_to_integer(is_atom(A), 1) + bool_to_integer(is_binary(B), 2) +
        bool_to_integer(is_integer(C), 4) +
        bool_to_integer(is_list(D), 8) + bool_to_integer(is_reference(E), 16) +
        bool_to_integer(is_tuple(F), 32) +
        bool_to_integer(is_number(F), 64) + bool_to_integer(is_pid(F), 128).
