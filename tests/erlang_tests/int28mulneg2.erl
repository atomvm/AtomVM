-module(int28mulneg2).

-export([start/0, mul/2, id/1]).

start() ->
    OK1 = mul(id(-33554431), id(2)),
    OK2 = mul(id(OK1), id(2)),
    OK3 = mul(id(OK2), id(-2)),
    OK3.

mul(A, B) ->
    try id(A) * id(B) of
        Any -> Any
    catch
        _:_ -> A div (B + 1)
    end.

id(I) ->
    I.
