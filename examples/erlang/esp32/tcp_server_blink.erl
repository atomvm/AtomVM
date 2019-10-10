-module(tcp_server_blink).

-export([start/0]).

-include("estdlib.hrl").

-define(PIN, 2).

start() ->
    erlang:display(start),
    try
        Self = self(),
        Config = [
            {sta, [
                {ssid, "myssid"},
                {psk,  "mypsk"},
                {connected, fun() -> Self ! connected end},
                {got_ip, fun(IpInfo) -> Self ! {ok, IpInfo} end},
                {disconnected, fun() -> Self ! disconnected end}
            ]}
        ],
        case network_fsm:start(Config) of
            ok ->
                wait_for_network();
            Error ->
                erlang:display(Error)
        end
    catch
        _:E ->
            erlang:display(E)
    end.


wait_for_network() ->
    receive
        connected ->
            erlang:display(connected),
            wait_for_network();
        {ok, IpInfo} ->
            erlang:display(IpInfo),
            tcp_server_start();
        disconnected ->
            erlang:display(disconnected)
    end,
    infinite_loop().

infinite_loop() ->
    ?TIMER:sleep(1000),
    infinite_loop().

tcp_server_start() ->
    case ?GEN_TCP:listen(44404, []) of
        {ok, ListenSocket} ->
            console:puts("Listening on port 44404.\n"),
            Gpio = gpio:open(),
            gpio:set_direction(Gpio, ?PIN, output),
            spawn(fun() -> accept(ListenSocket, Gpio) end);
        Error ->
            erlang:display(Error)
    end.

accept(ListenSocket, Gpio) ->
    console:puts("Waiting to accept connection...\n"),
    case ?GEN_TCP:accept(ListenSocket) of
        {ok, _Socket} ->
            console:puts("Accepted connection.\n"),
            spawn(fun() -> accept(ListenSocket, Gpio) end),
            echo(Gpio, 0);
        Error ->
            erlang:display(Error)
    end.

echo(Gpio, PinState) ->
    console:puts("Waiting to receive data...\n"),
    gpio:set_level(Gpio, ?PIN, PinState),
    receive
        {tcp_closed, _Socket} ->
            console:puts("Closed connection.\n"),
            ok;
        {tcp, Socket, Packet} ->
            erlang:display(Packet),
            console:puts("Sending packet back to client...\n"),
            ?GEN_TCP:send(Socket, Packet),
            echo(Gpio, 1 - PinState)
    end.

