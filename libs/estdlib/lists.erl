-module(lists).

-export([nth/2]).

nth(1, [H | _T]) ->
    H;

nth(Index, [_H | T]) when Index > 1 ->
    nth(Index - 1, T).
