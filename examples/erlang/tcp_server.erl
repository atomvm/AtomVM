-module(tcp_server).

-export([start/0]).

-include("estdlib.hrl").

start() ->
    case ?GEN_TCP:listen(44404, []) of
        {ok, ListenSocket} ->
            ?IO:format("Listening on ~p.~n", [local_address(ListenSocket)]),
            spawn(fun() -> accept(ListenSocket) end);
        Error ->
            ?IO:format("An error occurred listening: ~p~n", [Error])
    end.

accept(ListenSocket) ->
    ?IO:format("Waiting to accept connection...~n"),
    case ?GEN_TCP:accept(ListenSocket) of
        {ok, Socket} ->
            ?IO:format("Accepted connection.  local: ~p peer: ~p~n", [local_address(Socket), peer_address(Socket)]),
            spawn(fun() -> accept(ListenSocket) end),
            echo();
        Error ->
            ?IO:format("An error occurred accepting connection: ~p~n", [Error])
    end.

echo() ->
    ?IO:format("Waiting to receive data...~n"),
    receive
        {tcp_closed, _Socket} ->
            ?IO:format("Connection closed.~n"),
            ok;
        {tcp, Socket, Packet} ->
            ?IO:format("Received packet ~p from ~p.  Echoing back...~n", [Packet, peer_address(Socket)]),
            ?GEN_TCP:send(Socket, Packet),
            echo()
    end.

local_address(Socket) ->
    to_string(?INET:sockname(Socket)).

peer_address(Socket) ->
    to_string(?INET:peername(Socket)).

to_string({{A,B,C,D}, Port}) ->
    ?IO_LIB:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]).
