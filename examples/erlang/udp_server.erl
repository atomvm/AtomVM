-module(udp_server).

-export([start/0]).

start() ->
    case gen_udp:open(44404, [{active, true}]) of
        {ok, Socket} ->
            io:format("Opened UDP socket on ~p.~n", [local_address(Socket)]),
            active_loop();
        Error ->
            io:format("An error occurred opening UDP socket: ~p~n", [Error])
    end.

active_loop() ->
    io:format("Waiting to receive data...~n"),
    receive
        {udp, _Socket, Address, Port, Packet} ->
            io:format("Received UDP packet ~p from ~p~n", [Packet, to_string({Address, Port})])
    end,
    active_loop().

local_address(Socket) ->
    {ok, SockName} = inet:sockname(Socket),
    to_string(SockName).

to_string({{A,B,C,D}, Port}) ->
    io_lib:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]).
