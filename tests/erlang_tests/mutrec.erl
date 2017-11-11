-module(mutrec).

-export([start/0, rec4/1, willnotbecalled/2]).

start() ->
    rec2(2) + rec3(3).

rec2(X) -> X + 1.

rec3(X) -> rec4(X - 1).

rec4(0) -> 3;
rec4(X) -> rec3(X - 1).

willnotbecalled(0, O) -> 2 - O.
