-module(tcp_client).

-export([start/0]).

start() ->
    Address = "localhost",
    Port = 44404,
    case gen_tcp:connect(Address, Port, []) of
        {ok, Socket} ->
            io:format("Connected to ~p from ~p~n", [peer_address(Socket), local_address(Socket)]),
            loop(Socket);
        Error ->
            io:format("An error occurred connecting: ~p~n", [Error])
    end.

loop(Socket) ->
    SendPacket = <<":アトムＶＭ">>,
    case gen_tcp:send(Socket, SendPacket) of
        ok ->
            io:format("Sent ~p to ~p\n", [SendPacket, peer_address(Socket)]),
            receive
                {tcp_closed, _Socket} ->
                    io:format("Connection closed.~n"),
                    ok;
                {tcp, _Socket, ReceivedPacket} ->
                    io:format("Received ~p from ~p~n", [ReceivedPacket, peer_address(Socket)]),
                    timer:sleep(1000),
                    loop(Socket)
            end;
        Error ->
            io:format("An error occurred sending a packet: ~p~n", [Error])
    end.

local_address(Socket) ->
    to_string(inet:sockname(Socket)).

peer_address(Socket) ->
    to_string(inet:peername(Socket)).

to_string({{A,B,C,D}, Port}) ->
    io_lib:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]).
