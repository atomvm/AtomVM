-module(floatsubovf).

-export([start/0]).

start() ->
    id(sub(id(-1.7976931348623158e+308), id(1.7976931348623158e+308))).

sub(A, B) ->
    try id(A) - id(B) of
        C -> C
    catch
        error:badarith -> -2;
        _:_ -> -1
    end.

id(I) ->
    I.
