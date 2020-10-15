-module(test_atom_ordering).

-export([start/0, sort/1, insert/2, check/1, pow/2]).

start() ->
    Sorted = sort([aaa, aa, b, a, z, c, d, dd, test_atom_ordering, start, sort, erlang]),
    check(Sorted) +
        bool_to_n(Sorted < [zzzzz]) * 2 +
        bool_to_n(Sorted > {zzzzz}) * 4.

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

check(T) ->
    Expected = [a, aa, aaa, b, c, d, dd, erlang, sort, start, test_atom_ordering, z],
    check(T, Expected).

pow(_N, 0) ->
    1;
pow(N, M) ->
    N * pow(N, M - 1).

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
