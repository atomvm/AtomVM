-module (makelist_test).
-export([start/0,makelist/2,sumlist/2]).

start() ->
    makelist("hello", []).

makelist([H | T], L) ->
    makelist(T, [H] ++ L);
makelist([], L) ->
    sumlist(L, 0).

sumlist([H | T], Acc) ->
    sumlist(T, Acc + H);
sumlist([], Acc) ->
    Acc.

