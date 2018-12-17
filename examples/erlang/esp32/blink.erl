-module (blink).
-export([start/0]).

start() ->
    GPIO = gpio:open(),
    gpio:set_direction(GPIO, 2, output),
    loop(GPIO, off).

loop(GPIO, off) ->
    gpio:set_level(GPIO, 2, 0),
    timer:sleep(1000),
    loop(GPIO, on);
loop(GPIO, on) ->
    gpio:set_level(GPIO, 2, 1),
    timer:sleep(1000),
    loop(GPIO, off).
