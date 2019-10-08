-module(tcp_client).

-export([start/0]).

-include("estdlib.hrl").

start() ->
    console:puts("Connecting...\n"),
    Active = true,
    case ?GEN_TCP:connect("www.example.com", 80, [{active, Active}, {buffer, 1024}]) of
        {ok, Socket} ->
            erlang:display({connected, Socket}),
            case ?GEN_TCP:send(Socket, "GET / HTTP/1.0\r\n\r\n") of
                ok ->
                    case Active of 
                        true -> active_receive_data(); 
                        _ -> passive_receive_data(Socket) 
                    end;
                Error ->
                    erlang:display(Error)
            end;
        Error ->
            erlang:display(Error)
    end.

active_receive_data() ->
    receive
        {tcp_closed, _Socket} ->
            erlang:display(closed),
            ok;
        {tcp, _Socket, Packet} ->
            erlang:display(Packet),
            active_receive_data()
    end.

passive_receive_data(Socket) ->
    case ?GEN_TCP:recv(Socket, 128) of
        {error, closed} ->
            ok;
        {ok, Packet} ->
            erlang:display(Packet),
            passive_receive_data(Socket);
        Error ->
            erlang:display(Error)
    end.
