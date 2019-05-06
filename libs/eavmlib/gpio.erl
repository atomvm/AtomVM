-module(gpio).

-export([open/0, read/2, set_direction/3, set_level/3, set_int/3]).

open() ->
    open_port({spawn, "gpio"}, []).

read(GPIO, GPIONum) ->
    GPIO ! {self(), read, GPIONum},
    receive
        Ret ->
            Ret
    end.

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

set_int(GPIO, GPIONum, Trigger) ->
    GPIO ! {self(), set_int, GPIONum, Trigger},
    receive
        Ret ->
            Ret
    end.
