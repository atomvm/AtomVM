-module (test_echo_driver).
-export([start/0, do_open_port/2]).

start() ->
    Port = do_open_port("echo", nil),
    Port ! {self(), 42},
    receive
        RecValue ->
            RecValue * 2
    end.

do_open_port(PortName, Param) ->
    open_port({spawn, PortName}, Param).
