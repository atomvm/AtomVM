-module(minusone).

-export([start/0, sub/2, id/1]).

start() ->
    OK1 = sub(id(-134217727), id(1)),
    OK2 = sub(id(OK1), id(1)),
    OK2.

sub(A, B) ->
    try id(A) - id(B) of
        Any -> Any
    catch
        _:_ -> A div (B + 1)
    end.

id(I) ->
    I.
