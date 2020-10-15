-module(guards4).

-export([start/0, comp/2]).

start() ->
    comp(3, 3) * 5 + comp(1, 3).

comp(A, B) when A /= B ->
    1;
comp(A, B) when A =/= B ->
    2;
comp(_A, _B) ->
    3.
