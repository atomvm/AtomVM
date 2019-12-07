-module(tcp_client).

-export([start/0]).

-include("estdlib.hrl").

start() ->
    Address = "localhost",
    Port = 44404,
    case ?GEN_TCP:connect(Address, Port, []) of
        {ok, Socket} ->
            ?IO:format("Connected to ~p from ~p~n", [peer_address(Socket), local_address(Socket)]),
            loop(Socket);
        Error ->
            ?IO:format("An error occurred connecting: ~p~n", [Error])
    end.

loop(Socket) ->
    SendPacket = <<":アトムＶＭ">>,
    case ?GEN_TCP:send(Socket, SendPacket) of
        ok ->
            ?IO:format("Sent ~p to ~p\n", [SendPacket, peer_address(Socket)]),
            receive
                {tcp_closed, _Socket} ->
                    ?IO:format("Connection closed.~n"),
                    ok;
                {tcp, _Socket, ReceivedPacket} ->
                    ?IO:format("Received ~p from ~p~n", [ReceivedPacket, peer_address(Socket)]),
                    ?TIMER:sleep(1000),
                    loop(Socket)
            end;
        Error ->
            ?IO:format("An error occurred sending a packet: ~p~n", [Error])
    end.

local_address(Socket) ->
    to_string(?INET:sockname(Socket)).

peer_address(Socket) ->
    to_string(?INET:peername(Socket)).

to_string({{A,B,C,D}, Port}) ->
    ?IO_LIB:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]).
