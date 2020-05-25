-module(tcp_client_esp32).

-export([start/0]).

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
            tcp_client_start();
        Error ->
            io:format("An error occurred starting network: ~p~n", [Error])
    end.

tcp_client_start() ->
    case gen_tcp:connect("www.example.com", 80, [{active, true}]) of
        {ok, Socket} ->
            io:format("Connected to ~p from ~p~n", [peer_address(Socket), local_address(Socket)]),
            case gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n") of
                ok ->
                    active_receive_data();
                Error ->
                    io:format("An error occurred sending a packet: ~p~n", [Error])
            end;
        Error ->
            io:format("An error occurred connecting: ~p~n", [Error])
    end.

active_receive_data() ->
    receive
        {tcp_closed, _Socket} ->
            io:format("Connection closed.~n"),
            ok;
        {tcp, Socket, Packet} ->
            io:format("Received ~p from ~p~n", [Packet, peer_address(Socket)]),
            active_receive_data()
    end.

local_address(Socket) ->
    {ok, SockName} = inet:sockname(Socket),
    to_string(SockName).

peer_address(Socket) ->
    {ok, Peername} = inet:peername(Socket),
    to_string(Peername).

to_string({{A,B,C,D}, Port}) ->
    io_lib:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]);
to_string({A,B,C,D}) ->
    io_lib:format("~p.~p.~p.~p", [A,B,C,D]).
