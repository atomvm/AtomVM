-module(udp_server_blink).

-export([start/0]).

-define(PIN, 2).
-define(ON, 1).
-define(OFF, 0).

start() ->
    Creds = [
        {ssid, esp:nvs_get_binary(atomvm, sta_ssid, <<"myssid">>)},
        {psk,  esp:nvs_get_binary(atomvm, sta_psk, <<"mypsk">>)}
    ],
    case network_fsm:wait_for_sta(Creds, 30000) of
        {ok, {Address, Netmask, Gateway}} ->
            io:format(
                "Acquired IP address: ~p Netmask: ~p Gateway: ~p~n",
                [to_string(Address), to_string(Netmask), to_string(Gateway)]
            ),
            udp_server_start();
        Error ->
            io:format("An error occurred starting network: ~p~n", [Error])
    end.


udp_server_start() ->
    case gen_udp:open(44404, [{active, true}]) of
        {ok, Socket} ->
            io:format("Opened UDP socket on ~p.~n", [local_address(Socket)]),
            Gpio = gpio:open(),
            gpio:set_direction(Gpio, ?PIN, output),
            loop(Socket, Gpio, 0);
        Error ->
            io:format("An error occurred opening UDP socket: ~p~n", [Error])
    end.


loop(Socket, Gpio, PinState) ->
    io:format("Waiting to receive data...~n"),
    gpio:set_level(Gpio, ?PIN, PinState),
    receive
        {udp, _Socket, Address, Port, Packet} ->
            io:format("Received UDP packet ~p from ~p~n", [Packet, to_string({Address, Port})])
    end,
    loop(Socket, Gpio, 1 - PinState).

local_address(Socket) ->
    to_string(inet:sockname(Socket)).

to_string({{A,B,C,D}, Port}) ->
    io_lib:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]);
to_string({A,B,C,D}) ->
    io_lib:format("~p.~p.~p.~p", [A,B,C,D]).
