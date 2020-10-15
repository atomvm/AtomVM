-module(test_tuples_ordering).

-export([start/0, sort/1, insert/2, check/1]).

start() ->
    Sorted = sort([{7, 9}, {1, 2}, {5, 5, 1}, {1, 2, 3}, {}, {0}, {9, 9}]),
    check(Sorted) +
        bool_to_n(Sorted > [{}]) * 2 +
        bool_to_n(Sorted < [{9}]) * 4.

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

check(T) when T == [{}, {0}, {1, 2}, {7, 9}, {9, 9}, {1, 2, 3}, {5, 5, 1}] ->
    1;
check(_T) ->
    0.

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
