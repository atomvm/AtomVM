-module(test_funs6).

-export([start/0, sumeach/3, g/1, id/1]).

start() ->
    C = g(four),
    sumeach(fun(_V) -> C end, [1, 2, 3, 4], 0).

sumeach(F, [H|T], Acc) ->
    R = F(H),
    sumeach(F, T, R + Acc);

sumeach(_F, [], Acc) ->
    Acc.

g(zero) ->
    0;

g(four) ->
    4;

g(five) ->
    5.

id(I) ->
    I.
