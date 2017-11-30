-module (send_receive).
-export([start/0, send_test/2]).

start() ->
    send_test(9, self()),
    receive
        Value -> Value * 2
    end.

send_test(V, P) ->
    P ! V.
