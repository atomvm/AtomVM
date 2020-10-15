-module(test_funs5).

-export([start/0, sumeach/3, g/1, id/1]).

start() ->
    C = g(four),
    sumeach([1, 2, 3, 4], fun(V) -> V * C end, 0).

sumeach([H | T], F, Acc) ->
    try F(H, g(zero)) of
        Result -> sumeach(F, T, Result + Acc)
    catch
        error:{badarity, _f} ->
            1000;
        error:badarity ->
            1000;
        _:_ ->
            3000
    end;
sumeach([], _F, Acc) ->
    Acc.

g(zero) ->
    0;
g(four) ->
    4;
g(five) ->
    5.

id(I) ->
    I.
