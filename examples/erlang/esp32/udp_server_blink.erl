-module(udp_server_blink).

-export([start/0]).

-include("estdlib.hrl").

-define(PIN, 2).
-define(ON, 1).
-define(OFF, 0).

start() ->
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
            wait_for_message();
        Error ->
            erlang:display(Error)
    end.


wait_for_message() ->
    receive
        connected ->
            erlang:display(connected);
        {ok, IpInfo} ->
            erlang:display(IpInfo),
            udp_server_start();
        disconnected ->
            erlang:display(disconnected)
    after 15000 ->
        ok
    end,
    wait_for_message().


udp_server_start() ->
    console:puts("Opening socket 44444 ...\n"),
    Socket = ?GEN_UDP:open(44444),
    erlang:display(Socket),
    Gpio = gpio:open(),
    gpio:set_direction(Gpio, ?PIN, output),
    udp_server_loop(Socket, Gpio, off).


udp_server_loop(Socket, Gpio, PinState) ->
    console:puts("Waiting to receive data...\n"),
    case ?GEN_UDP:recv(Socket, 1000) of
        {ok, RecvData} ->
            {Address, Port, Packet} = RecvData,
            console:puts("Address: "),  erlang:display(Address),
            console:puts("Port: "),     erlang:display(Port),
            console:puts("Packet: "),   erlang:display(Packet),
            case PinState of
                on ->
                    gpio:set_level(Gpio, ?PIN, ?OFF),
                    udp_server_loop(Socket, Gpio, off);
                off ->
                    gpio:set_level(Gpio, ?PIN, ?ON),
                    udp_server_loop(Socket, Gpio, on)
            end;
        {error, Reason} ->
            console:puts("An error ocurred: "), erlang:display(Reason)
    end.
