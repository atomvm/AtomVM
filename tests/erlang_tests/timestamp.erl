-module(timestamp).
-export([start/0]).

start() ->
    test_minutes(erlang:system_time(minute)).

test_minutes(M) when is_integer(M) andalso M > 0 ->
    1.
