-module(floorfloatovf).

-export([start/0]).

start() ->
    to_int(id(id([1.0e+65, 0]))).

to_int(A) ->
    try floor(id(A)) of
        B -> B
    catch
        error:overflow -> -1;
        _:_ -> 1
    end.

id([I | _T]) -> id(I);
id(I) -> I.
