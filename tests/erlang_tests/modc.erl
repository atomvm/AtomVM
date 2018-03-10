-module(modc).

-export([test/1]).

test(Val) ->
    moda:test2(Val * 2, dec).
