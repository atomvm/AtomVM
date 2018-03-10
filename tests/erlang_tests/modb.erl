-module(modb).

-export([test/0]).

test() ->
    modc:test(moda:test1()).
