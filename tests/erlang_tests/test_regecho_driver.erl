-module (test_regecho_driver).
-export([start/0, do_open_port/2, echo/1]).

start() ->
    register(echo, do_open_port("echo", [])),
    length(echo("Hello World")).

do_open_port(PortName, Param) ->
    open_port({spawn, PortName}, Param).

echo(SendValue) ->
    whereis(echo) ! {self(), SendValue},
    receive
        Value ->
            Value
    end.
