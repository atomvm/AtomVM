-module (blink).
-export([start/0, do_open_port/2, set_direction/3, set_level/3]).

start() ->
    GPIO = do_open_port("gpio", []),
    set_direction(GPIO, 17, output),
    loop(GPIO, 0).

loop(GPIO, 0) ->
    set_level(GPIO, 17, 0),
    sleep(1000),
    loop(GPIO, 1);
loop(GPIO, 1) ->
    set_level(GPIO, 17, 1),
    sleep(1000),
    loop(GPIO, 0).

do_open_port(PortName, Param) ->
    open_port({spawn, PortName}, Param).

set_direction(GPIO, GPIONum, Direction) ->
    GPIO ! {self(), set_direction, GPIONum, Direction},
    receive
        Ret ->
            Ret
    end.

set_level(GPIO, GPIONum, Level) ->
    GPIO ! {self(), set_level, GPIONum, Level},
    receive
        Ret ->
            Ret
    end.

sleep(T) ->
    receive
        after T -> ok
    end.
