-module(udp_server_blink).

-export([start/0]).

-include("estdlib.hrl").
-include("atomvm.hrl").

-define(PIN, 2).
-define(ON, 1).
-define(OFF, 0).

start() ->
    Creds = [
        {ssid, esp:nvs_get_binary(?ATOMVM_NVS_NS, ?ATOMVM_NVS_STA_SSID, <<"myssid">>)},
        {psk,  esp:nvs_get_binary(?ATOMVM_NVS_NS, ?ATOMVM_NVS_STA_PSK, <<"mypsk">>)}
    ],
    case network_fsm:wait_for_sta(Creds, 30000) of
        {ok, {Address, Netmask, Gateway}} ->
            ?IO:format(
                "Acquired IP address: ~p Netmask: ~p Gateway: ~p~n", 
                [to_string(Address), to_string(Netmask), to_string(Gateway)]
            ),
            udp_server_start();
        Error ->
            ?IO:format("An error occurred starting network: ~p~n", [Error])
    end.


udp_server_start() ->
    case ?GEN_UDP:open(44404, [{active, true}]) of
        {ok, Socket} ->
            ?IO:format("Opened UDP socket on ~p.~n", [local_address(Socket)]),
            Gpio = gpio:open(),
            gpio:set_direction(Gpio, ?PIN, output),
            loop(Socket, Gpio, 0);
        Error ->
            ?IO:format("An error occurred opening UDP socket: ~p~n", [Error])
    end.


loop(Socket, Gpio, PinState) ->
    ?IO:format("Waiting to receive data...~n"),
    gpio:set_level(Gpio, ?PIN, PinState),
    receive
        {udp, _Socket, Address, Port, Packet} -> 
            ?IO:format("Received UDP packet ~p from ~p~n", [Packet, to_string({Address, Port})])
    end,
    loop(Socket, Gpio, 1 - PinState).

local_address(Socket) ->
    to_string(?INET:sockname(Socket)).

to_string({{A,B,C,D}, Port}) ->
    ?IO_LIB:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]);
to_string({A,B,C,D}) ->
    ?IO_LIB:format("~p.~p.~p.~p", [A,B,C,D]).
