-module(udp_server_blink).

-export([start/0]).

-include("estdlib.hrl").

-define(PIN, 2).
-define(ON, 1).
-define(OFF, 0).

start() ->
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
                wait_for_message();
            Error ->
                erlang:display(Error)
        end
    catch
        _:E ->
            erlang:display(E)
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
    {ok, Socket} = ?GEN_UDP:open(44444, [{active, false}]),
    erlang:display(Socket),
    Gpio = gpio:open(),
    gpio:set_direction(Gpio, ?PIN, output),
    console:puts("Waiting to receive data...\n"),
    passive_loop(Socket, Gpio, 0).


passive_loop(Socket, Gpio, PinState) ->
    gpio:set_level(Gpio, ?PIN, PinState),
    NewPinState = case ?GEN_UDP:recv(Socket, 128) of
        {ok, RecvData} ->
            erlang:display(RecvData),
            1 - PinState;
        ErrorReason ->
            erlang:display(ErrorReason),
            PinState
    end,
    passive_loop(Socket, Gpio, NewPinState).
