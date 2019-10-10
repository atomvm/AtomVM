-module(tcp_client).

-export([start/0]).

-include("estdlib.hrl").

start() ->
    console:puts("Connecting...\n"),
    Address = "localhost",
    Port = 44404,
    case ?GEN_TCP:connect(Address, Port, []) of
        {ok, Socket} ->
            console:puts("Connected.\n"),
            loop(Socket);
        Error ->
            erlang:display(Error)
    end.

loop(Socket) ->
    console:puts("Sending message to server...\n"),
    case ?GEN_TCP:send(Socket, <<":アトムＶＭ">>) of
        ok ->
            receive
                {tcp_closed, _Socket} ->
                    console:puts("Closed.\n"),
                    ok;
                {tcp, _Socket, Packet} ->
                    console:puts("Received packet.\n"),
                    erlang:display(Packet),
                    ?TIMER:sleep(1000),
                    loop(Socket)
            end;
        Error ->
            erlang:display(Error)
    end.
