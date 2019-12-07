-module(tcp_client_esp32).

-export([start/0]).

-include("estdlib.hrl").
-include("atomvm.hrl").

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
            tcp_client_start();
        Error ->
            ?IO:format("An error occurred starting network: ~p~n", [Error])
    end.

tcp_client_start() ->
    case ?GEN_TCP:connect("www.example.com", 80, [{active, true}]) of
        {ok, Socket} ->
            ?IO:format("Connected to ~p from ~p~n", [peer_address(Socket), local_address(Socket)]),
            case ?GEN_TCP:send(Socket, "GET / HTTP/1.0\r\n\r\n") of
                ok ->
                    active_receive_data();
                Error ->
                    ?IO:format("An error occurred sending a packet: ~p~n", [Error])
            end;
        Error ->
            ?IO:format("An error occurred connecting: ~p~n", [Error])
    end.

active_receive_data() ->
    receive
        {tcp_closed, _Socket} ->
            ?IO:format("Connection closed.~n"),
            ok;
        {tcp, Socket, Packet} ->
            ?IO:format("Received ~p from ~p~n", [Packet, peer_address(Socket)]),
            active_receive_data()
    end.

local_address(Socket) ->
    to_string(?INET:sockname(Socket)).

peer_address(Socket) ->
    to_string(?INET:peername(Socket)).

to_string({{A,B,C,D}, Port}) ->
    ?IO_LIB:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]);
to_string({A,B,C,D}) ->
    ?IO_LIB:format("~p.~p.~p.~p", [A,B,C,D]).
