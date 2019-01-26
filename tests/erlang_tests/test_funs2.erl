-module(test_funs2).

-export([start/0, sumeach/3]).

start() ->
    sumeach(fun(V, Acc) -> V * 2 + Acc end, [1, 2, 3, 4], 0).

sumeach(F, [H|T], Acc) ->
    R = F(H, Acc),
    sumeach(F, T, R + Acc);

sumeach(_F, [], Acc) ->
    Acc.
