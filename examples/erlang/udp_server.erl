-module(udp_server).

-export([start/0]).

-include("estdlib.hrl").

start() ->
    case ?GEN_UDP:open(44404, [{active, true}]) of
        {ok, Socket} ->
            ?IO:format("Opened UDP socket on ~p.~n", [local_address(Socket)]),
            active_loop();
        Error ->
            ?IO:format("An error occurred opening UDP socket: ~p~n", [Error])
    end.

active_loop() ->
    ?IO:format("Waiting to receive data...~n"),
    receive
        {udp, _Socket, Address, Port, Packet} -> 
            ?IO:format("Received UDP packet ~p from ~p~n", [Packet, to_string({Address, Port})])
    end,
    active_loop().

local_address(Socket) ->
    to_string(?INET:sockname(Socket)).

to_string({{A,B,C,D}, Port}) ->
    ?IO_LIB:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]).
