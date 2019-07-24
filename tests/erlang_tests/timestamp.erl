-module(timestamp).
-export([start/0]).

start() ->
    test_seconds(erlang:system_time(second)).

test_seconds(M) when is_integer(M) andalso M > 0 ->
    1.
