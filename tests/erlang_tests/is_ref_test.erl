-module (is_ref_test).
-export([start/0, func/2]).

start() ->
    func(1, make_ref()) + func(0, 9).

func(A, B) when is_reference(B) ->
    A * 2;
func(_A, B) when is_integer(B) ->
    1.
