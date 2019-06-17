-module(plusone3).

-export([start/0, add/2, id/1]).

start() ->
    OK1 = add(id(134217725), id(0)),
    OK2 = add(id(OK1), id(1)),
    OK3 = add(id(OK2), id(1)),
    OK4 = add(id(OK3), id(0)),
    OK5 = add(id(OK4), id(1)),
    OK6 = add(id(OK5), id(1)),
    OK7 = add(id(OK6), idN(1)),
    OK8 = add(id(OK7), idN(1)),
    OK9 = add(id(OK8), idN(1)),
    OK9.

add(A, B) ->
    try id(A) + id(B) of
        Any -> Any
    catch
        _:_ -> A div (B + 1)
    end.

id(I) ->
    I.

idN(I) ->
    -id(I).
