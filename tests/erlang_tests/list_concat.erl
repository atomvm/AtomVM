-module(list_concat).

-export([start/0, concat/2]).

start() ->
    sum_list(concat2(concat([1, 2, 3], [8, 9, 10, 11, 12]))).

concat(A, B) ->
    A ++ [4, 5, 6, 7] ++ B.

concat2(A) ->
    A ++ [13].

sum_list(List) ->
    sum_list_w(0, List, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]).

sum_list_w(Acc, [Head | Tail], [WHead | WTail]) ->
    sum_list_w(Acc + Head * WHead, Tail, WTail);
sum_list_w(Acc, [], _W) ->
    Acc.
