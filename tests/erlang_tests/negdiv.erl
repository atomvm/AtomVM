-module(negdiv).

-export([start/0, divfunc/2, id/1]).

start() ->
    divfunc(id(-134217728), id(-1)).

divfunc(A, B) ->
    try id(A) div id(B) of
        Value -> Value
    catch
        _:_ -> A - B * 10
    end.

id(A) ->
    A.
