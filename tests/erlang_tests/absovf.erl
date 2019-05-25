-module(absovf).

-export([start/0, absfunc/2, id/1]).

start() ->
    absfunc(id(-134217728), id(1)).

absfunc(A, B) ->
    try abs(id(A)) of
        Value -> Value * B
    catch
        _:_ -> A + B * 10
    end.

id(A) ->
    A.
