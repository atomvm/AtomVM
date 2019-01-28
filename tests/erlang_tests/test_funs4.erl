-module(test_funs4).

-export([start/0, sumeach/3, g/1, id/1]).

start() ->
    C = g(four),
    sumeach([1, 2, 3, 4], fun(V) -> id(V rem C) end, 0).

sumeach([H|T], F, Acc) ->
    R = F(H),
    sumeach(T, F, R + Acc);

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
