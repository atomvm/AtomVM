-module(tcp_server).

-export([start/0]).

-include("estdlib.hrl").

start() ->
    case ?GEN_TCP:listen(44404, []) of
        {ok, ListenSocket} ->
            console:puts("Listening on port 44404.\n"),
            erlang:display(?INET:sockname(ListenSocket)),
            spawn(fun() -> accept(ListenSocket) end);
        Error ->
            erlang:display(Error)
    end.

accept(ListenSocket) ->
    console:puts("Waiting to accept connection...\n"),
    case ?GEN_TCP:accept(ListenSocket) of
        {ok, Socket} ->
            console:puts("Accepted connection.\n"),
            erlang:display({?INET:peername(Socket), ?INET:sockname(Socket)}),
            spawn(fun() -> accept(ListenSocket) end),
            echo();
        Error ->
            erlang:display(Error)
    end.

echo() ->
    console:puts("Waiting to receive data...\n"),
    receive
        {tcp_closed, _Socket} ->
            console:puts("Closed connection.\n"),
            ok;
        {tcp, Socket, Packet} ->
            erlang:display(Packet),
            console:puts("Sending packet back to client...\n"),
            ?GEN_TCP:send(Socket, Packet),
            echo()
    end.

