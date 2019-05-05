-module(gpio).

-export([open/0, set_direction/3, set_level/3, set_int/2]).

open() ->
    open_port({spawn, "gpio"}, []).

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

set_int(GPIO, GPIONum) ->
    GPIO ! {self(), set_int, GPIONum},
    receive
        Ret ->
            Ret
    end.
