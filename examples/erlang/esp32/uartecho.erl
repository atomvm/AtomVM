-module(uartecho).
-export([start/0]).

start() ->
    UART = uart:open("UART0", []),
    loop(UART).

loop(UART) ->
    {ok, B} = uart:read(UART),
    uart:write(UART, [<<"Echo: ">>, B, <<"\n">>]),
    loop(UART).
