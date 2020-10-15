-module(hello_world).

-export([start/0, do_open_port/2, write/2]).

start() ->
    Console = do_open_port("console", []),
    result_to_int(write(Console, "--- Hello World\n")).

do_open_port(PortName, Param) ->
    open_port({spawn, PortName}, Param).

write(Console, String) ->
    Ref = make_ref(),
    Console ! {self(), Ref, {puts, String}},
    receive
        {Ref, ReturnStatus} ->
            ReturnStatus
    end.

result_to_int(ok) ->
    10;
result_to_int(_) ->
    20.
