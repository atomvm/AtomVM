-module (hello_world).
-export([start/0, do_open_port/2, write/2]).

start() ->
    Console = do_open_port("console", []),
    write(Console, "Hello World\n").

do_open_port(PortName, Param) ->
    open_port({spawn, PortName}, Param).

write(Console, String) ->
    Console ! {self(), String},
    receive
        ReturnStatus ->
            ReturnStatus
    end.
