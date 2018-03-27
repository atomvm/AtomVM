-module(guards1).

-export([start/0, comp/2]).

start() ->
    comp(3, 3) - comp(1, 2)*2 + comp(3, 1).

comp(A, B) when A > B->
    1;
comp(A, B) when A < B->
    20;
comp(A, B) when A >= B->
    300;
comp(A, B) when A =< B->
    1000.
