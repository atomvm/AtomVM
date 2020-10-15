-module(test_types_ordering).

-export([start/0, sort/1, insert/2, check/1]).

start() ->
    Sorted = sort([{1}, [], {}, <<"bar">>, 1, [1, 2], {1, 2}, foo]),
    check(Sorted).

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

check(T) when T == [1, foo, {}, {1}, {1, 2}, [], [1, 2], <<"bar">>] ->
    1;
check(_T) ->
    0.

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
