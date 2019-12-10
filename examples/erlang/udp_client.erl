-module(udp_client).

-export([start/0]).

-include("estdlib.hrl").

start() ->
    case ?GEN_UDP:open(0) of
        {ok, Socket} ->
            ?IO:format("Opened UDP socket on ~p.~n", [local_address(Socket)]),
            loop(Socket);
        Error ->
            ?IO:format("An error occurred opening UDP socket: ~p~n", [Error])
    end.

loop(Socket) ->
    Packet = <<":アトムＶＭ">>,
    case ?GEN_UDP:send(Socket, {127,0,0,1}, 44404, Packet) of
        ok ->
            ?IO:format("Sent ~p~n", [Packet]);
        Error ->
            ?IO:format("An error ocurred sending a packet: ~p~n", [Error])
    end,
    ?TIMER:sleep(1000),
    loop(Socket).

local_address(Socket) ->
    to_string(?INET:sockname(Socket)).

to_string({{A,B,C,D}, Port}) ->
    ?IO_LIB:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]).
