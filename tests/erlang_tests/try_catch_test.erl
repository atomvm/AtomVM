-module(try_catch_test).

-export([start/0, fail/1, mayfail/1]).

start() ->
    mayfail(89) + mayfailsafe(42).

mayfail(Value) ->
    fail(Value) + 10.

mayfailsafe(Value) ->
    try mayfail(Value) + 5 of
        _Any -> 1
    catch
        error:badarg -> 0;
        _:_ -> -1
    end.

fail(X) ->
    89 = X,
    100.
