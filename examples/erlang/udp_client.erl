-module(udp_client).

-export([start/0]).

start() ->
    case gen_udp:open(0) of
        {ok, Socket} ->
            io:format("Opened UDP socket on ~p.~n", [local_address(Socket)]),
            loop(Socket);
        Error ->
            io:format("An error occurred opening UDP socket: ~p~n", [Error])
    end.

loop(Socket) ->
    Packet = <<":アトムＶＭ">>,
    case gen_udp:send(Socket, {127,0,0,1}, 44404, Packet) of
        ok ->
            io:format("Sent ~p~n", [Packet]);
        Error ->
            io:format("An error ocurred sending a packet: ~p~n", [Error])
    end,
    timer:sleep(1000),
    loop(Socket).

local_address(Socket) ->
    to_string(inet:sockname(Socket)).

to_string({{A,B,C,D}, Port}) ->
    io_lib:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]).
