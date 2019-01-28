-module(test_funs0).

-export([start/0, sumeach/3]).

start() ->
    sumeach(fun(V) -> V * 2 end, [1, 2, 3, 4], 0).

sumeach(F, [H|T], Acc) ->
    R = F(H),
    sumeach(F, T, R + Acc);

sumeach(_F, [], Acc) ->
    Acc.
