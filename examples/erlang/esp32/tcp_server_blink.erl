-module(tcp_server_blink).

-export([start/0]).

-include("estdlib.hrl").
-include("atomvm.hrl").

-define(PIN, 2).

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
            tcp_server_start();
        Error ->
            ?IO:format("An error occurred starting network: ~p~n", [Error])
    end.

tcp_server_start() ->
    case ?GEN_TCP:listen(44404, [{active, true}]) of
        {ok, ListenSocket} ->
            ?IO:format("Listening on ~p.~n", [local_address(ListenSocket)]),
            Gpio = gpio:open(),
            gpio:set_direction(Gpio, ?PIN, output),
            spawn(fun() -> accept(ListenSocket, Gpio) end);
        Error ->
            ?IO:format("An error occurred listening: ~p~n", [Error])
    end.

accept(ListenSocket, Gpio) ->
    ?IO:format("Waiting to accept connection...~n"),
    case ?GEN_TCP:accept(ListenSocket) of
        {ok, Socket} ->
            ?IO:format("Accepted connection.  local: ~p peer: ~p~n", [local_address(Socket), peer_address(Socket)]),
            spawn(fun() -> accept(ListenSocket, Gpio) end),
            echo(Gpio, 0);
        Error ->
            ?IO:format("An error occurred accepting connection: ~p~n", [Error])
    end.

echo(Gpio, PinState) ->
    ?IO:format("Waiting to receive data...~n"),
    gpio:set_level(Gpio, ?PIN, PinState),
    receive
        {tcp_closed, _Socket} ->
            ?IO:format("Connection closed.~n"),
            ok;
        {tcp, Socket, Packet} ->
            ?IO:format("Received packet ~p from ~p.  Echoing back...~n", [Packet, peer_address(Socket)]),
            ?GEN_TCP:send(Socket, Packet),
            echo(Gpio, 1 - PinState)
    end.

local_address(Socket) ->
    to_string(?INET:sockname(Socket)).

peer_address(Socket) ->
    to_string(?INET:peername(Socket)).

to_string({{A,B,C,D}, Port}) ->
    ?IO_LIB:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]);
to_string({A,B,C,D}) ->
    ?IO_LIB:format("~p.~p.~p.~p", [A,B,C,D]).
