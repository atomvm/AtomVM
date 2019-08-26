-module(test_binaries_ordering).
-export([start/0, sort/1, insert/2, check/1]).

start() ->
    Sorted = sort([<<"Foo">>, <<"Bar">>, <<"Alice">>, <<"Bob">>, <<"lowercase">>]),
    Sorted2 = sort([<<"aaa">>, <<"aa">>, <<"aaz">>, <<"b">>, <<"a">>, <<"z">>, <<"c">>, <<"d">>, <<"dd">>, <<"test_binaries_ordering">>, <<"start">>, <<"sort">>, <<"erlang">>]),
    check(Sorted) +
    bool_to_n(Sorted > [<<"Aalice">>]) * 2 +
    bool_to_n(Sorted < [<<"Bar">>]) * 4 +
    check(Sorted2).

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

check(T) when T == [<<"a">>,<<"aa">>,<<"aaa">>,<<"aaz">>,<<"b">>,<<"c">>,<<"d">>,<<"dd">>,<<"erlang">>,<<"sort">>,<<"start">>,<<"test_binaries_ordering">>,<<"z">>] ->
    8;
check(T) when T == [<<"Alice">>, <<"Bar">>, <<"Bob">>, <<"Foo">>, <<"lowercase">>] ->
    1;
check(_T) ->
    0.

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
