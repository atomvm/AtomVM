-module(test_display).
-export([start/0]).

start() ->
    erlang:display([{a, 1}, {b, 2}, false, [1, 2], [], "hello world", <<"this is a binary">>, self(), make_ref()]),
    0.
