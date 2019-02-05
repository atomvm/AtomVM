-module(udp_server).

-export([start/0]).

-include("estdlib.hrl").

start() ->
    Console = console:start(),
    console:puts(Console, "Opening socket ...\n"),
    Socket = ?GEN_UDP:open(44444),
    erlang:display(Socket),
    loop(Console, Socket).


loop(Console, Socket) ->
    console:puts(Console, "Waiting to receive data...\n"),
    case ?GEN_UDP:recv(Socket, 1000) of
        {ok, RecvData} ->
            {Address, Port, Packet} = RecvData,
            console:puts(Console, "Address: "), erlang:display(Address),
            console:puts(Console, "Port: "), erlang:display(Port),
            console:puts(Console, "Packet: "), erlang:display(Packet),
            loop(Console, Socket);
        {error, Reason} ->
            console:puts(Console, "An error ocurred: "), erlang:display(Reason)
    end.
