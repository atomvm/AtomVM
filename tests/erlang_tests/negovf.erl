-module(negovf).

-export([start/0, negfunc/2, id/1]).

start() ->
    negfunc(id(-134217728), id(1)).

negfunc(A, B) ->
    try -id(A) of
        Value -> Value * B
    catch
        _:_ -> A + B * 10
    end.

id(A) ->
    A.
