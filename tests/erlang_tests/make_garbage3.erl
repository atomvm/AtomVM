-module(make_garbage3).

-export([start/0, sort/1, insert/2, check/1]).

start() ->
    check(sort([7, 1, 5])).

sort(L) ->
    sort(L, []).

sort([], Sorted) ->
    Sorted;
sort([H | Unsorted], Sorted) ->
    NextSorted = insert(Sorted, H),
    sort(Unsorted, NextSorted).

insert(L, I) ->
    insert(L, [], I).

insert([], HL, I) ->
    HL ++ [I];
insert([H | T], HL, I) when I < H ->
    HL ++ [I, H | T];
insert([H | T], HL, I) ->
    insert(T, HL ++ [H], I).

check(L) ->
    check(L, 0, 0).

check([], _, Acc) ->
    Acc;
check([H | T], 0, Acc) ->
    check(T, 1, Acc + H);
check([0 | T], 1, Acc) ->
    check(T, 2, Acc * 2);
check([H | T], 1, Acc) ->
    check(T, 2, Acc div H);
check([H | T], 2, Acc) ->
    check(T, 3, Acc - H);
check([0 | T], 3, Acc) ->
    check(T, 0, Acc - 1);
check([H | T], 3, Acc) ->
    check(T, 0, Acc * H).
