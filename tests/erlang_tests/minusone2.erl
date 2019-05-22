-module(minusone2).

-export([start/0, sub/2, id/1]).

start() ->
    OK1 = sub(id(2), id(1)),
    OK2 = sub(id(OK1), id(1)),
    OK3 = sub(id(OK2), id(1)),
    OK4 = sub(id(OK3), id(1)),
    OK5 = sub(id(OK4), id(1)),
    OK6 = sub(id(OK5), id(1)),
    OK6 * sub(id(5), id(1)).

sub(A, B) ->
    try id(A) - id(B) of
        Any -> Any
    catch
        _:_ -> A div (B + 1)
    end.

id(I) ->
    I.
