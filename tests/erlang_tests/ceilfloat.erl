-module(ceilfloat).

-export([start/0]).

start() ->
    to_int(id(id([-2.5, 0]))).

to_int(A) ->
    try ceil(id(A)) of
        B -> B
    catch
        error:badarg -> -1;
        _:_ -> 1
    end.

id([I | _T]) -> id(I);
id(I) -> I.
