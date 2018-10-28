-module(test_atom_to_list).
-export([start/0, f/1, g/1, compare_list/2]).

start() ->
    compare_list(f(hello_world), "hello" ++ g(world)).

f(A) when is_atom(A) ->
    atom_to_list(A).

compare_list([], []) ->
    1;

compare_list([H_A | T_A], [H_B | T_B]) when H_A == H_B ->
    compare_list(T_A, T_B);

compare_list(_A, _B) ->
    0.

g(world) ->
    "_world";

g(A) when is_atom(A) ->
    "?".
