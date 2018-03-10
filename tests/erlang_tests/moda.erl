-module(moda).

-export([start/0, test1/0, test2/2]).

start() ->
    modb:test() + 5.

test1() ->
    20.

test2(Val, inc) ->
    Val + 1;
test2(Val, dec) ->
    Val - 1;
test2(_Val, _) ->
    0.
