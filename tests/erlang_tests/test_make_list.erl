-module(test_make_list).

-export([start/0]).

start() ->
    length(make_list(a, b, c)).

make_list(A, B, C) ->
    [{foo, "bar"}, {gnu, "gnat"}, {a, A}, {b, B}, {c, C}].
