-module(test_pids_ordering).

-export([start/0, sort/1, insert/2, check/2]).

start() ->
    P1 = spawn(?MODULE, sort, [[]]),
    P2 = spawn(?MODULE, sort, [[1]]),
    P3 = spawn(?MODULE, sort, [[2, 1]]),
    P4 = spawn(?MODULE, sort, [[3]]),
    P5 = spawn(?MODULE, sort, [[4, 1]]),
    P6 = spawn(?MODULE, sort, [[]]),
    Sorted = sort([P3, P4, P1, P5, P2]),
    check(Sorted, [P1, P2, P3, P4, P5]) +
        bool_to_n(Sorted < [P6]) * 2 +
        bool_to_n(Sorted > {P6}) * 4.

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

check(T, Expected) when T == Expected ->
    1;
check(T, Expected) when T /= Expected ->
    0.

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
