-module(truncbadarg).

-export([start/0, id/1]).

start() ->
    to_int(id([id(2)])).

to_int(A) ->
    try trunc(id(A)) of
        Res -> Res
    catch
        error:badarg -> -1;
        _:_ -> 1
    end.

id(I) ->
    I.
