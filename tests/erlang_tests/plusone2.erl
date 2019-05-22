-module(plusone2).

-export([start/0, add/2, id/1]).

start() ->
    OK1 = add(id(-3), id(1)),
    OK2 = add(id(OK1), id(1)),
    OK3 = add(id(OK2), id(0)),
    OK4 = add(id(OK3), id(1)),
    OK5 = add(id(OK4), id(1)),
    OK5.

add(A, B) ->
    try id(A) + id(B) of
        Any -> Any
    catch
        _:_ -> A div (B + 1)
    end.

id(I) ->
    I.
