-module(gpio).

-export([open/0, read/2, set_direction/3, set_level/3, set_int/3, remove_int/2]).
-export([set_pin_mode/2, digital_write/2, digital_read/1, attach_interrupt/2, detach_interrupt/1]).

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

remove_int(GPIO, GPIONum) ->
    GPIO ! {self(), remove_int, GPIONum},
    receive
      Ret ->
        Ret
    end.

set_pin_mode(_GPIONum, _Mode) ->
    throw(nif_error).

digital_write(_GPIONum, _Level) ->
    throw(nif_error).

digital_read(_GPIONum) ->
    throw(nif_error).

attach_interrupt(GPIONum, Mode) ->
    set_int(open(), GPIONum, Mode).

detach_interrupt(GPIONum) ->
    remove_int(open(), GPIONum).
