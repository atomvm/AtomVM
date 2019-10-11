-module(floatmulovf).

-export([start/0]).

start() ->
    id(add(id(1.7976931348623158e+308), id(1.7976931348623158e+308))).

add(A, B) ->
    try id(A) * id(B) of
        C -> C
    catch
        error:badarith -> -2;
        _:_ -> -1
    end.

id(I) ->
    I.
