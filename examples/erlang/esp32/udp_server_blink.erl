-module(udp_server_blink).

-export([start/0]).

-include("estdlib.hrl").
-include("atomvm.hrl").

-define(PIN, 2).
-define(ON, 1).
-define(OFF, 0).

start() ->
    try
        Self = self(),
        Config = [
            {sta, [
                {ssid, esp:nvs_get_binary(?ATOMVM_NVS_NS, ?ATOMVM_NVS_STA_SSID, <<"myssid">>)},
                {psk,  esp:nvs_get_binary(?ATOMVM_NVS_NS, ?ATOMVM_NVS_STA_PSK, <<"mypsk">>)},
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
            erlang:display(connected);
        {ok, IpInfo} ->
            erlang:display(IpInfo),
            udp_server_start();
        disconnected ->
            erlang:display(disconnected)
    after 15000 ->
        ok
    end,
    wait_for_network().


udp_server_start() ->
    console:puts("Opening socket 44444 ...\n"),
    case ?GEN_UDP:open(44444, [{active, true}]) of
        {ok, Socket} ->
            erlang:display({Socket, ?INET:sockname(Socket)}),
            Gpio = gpio:open(),
            gpio:set_direction(Gpio, ?PIN, output),
            console:puts("Waiting to receive data...\n"),
            loop(Socket, Gpio, 0);
        ErrorReason ->
            erlang:display(ErrorReason)
    end.


loop(Socket, Gpio, PinState) ->
    gpio:set_level(Gpio, ?PIN, PinState),
    receive
        Msg ->
            erlang:display(Msg)
    end,
    loop(Socket, Gpio, 1 - PinState).
