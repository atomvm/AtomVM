-module (test_send_nif_and_echo).
-export([start/0, do_open_port/2, echo/1]).

start() ->
    register(echo, do_open_port(<<"echo">>, nil)),
    byte_size(echo(<<"Hello World">>)).

do_open_port(PortName, Param) ->
    open_port({spawn, PortName}, Param).

echo(SendValue) ->
    erlang:send(whereis(echo),{self(), SendValue}),
    receive
        Value ->
            Value
    end.
