-module(make_garbage4).

-export([start/0, sort/1, insert/2, check/1]).

start() ->
    check(sort([5, 7, 9, 15, 18, 22, 4, 11, 89, 94, 1, 0, 5, 8, 9, 3, 4, 5, 35, 12, 93, 29, 39, 29,
          22, 93, 23, 28, 98, 32, 32, 42, 91, 83, 38, 18, 98, 10, 12, 39, 14, 12, 93, 32, 23,
          93, 9, 13, 40, 34, 23, 23, 24, 42, 23, 23, 42, 90, 23, 32, 94, 42, 11, 32, 12, 12], [])).

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
