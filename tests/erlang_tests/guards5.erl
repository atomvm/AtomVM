-module(guards5).

-export([start/0, do_something/1]).

start() ->
    do_something(true) + do_something(false)*2.

do_something(A) when is_boolean(A) ->
    1;
do_something(_A) ->
    0.
