-module(udp_server).

-export([start/0]).

-include("estdlib.hrl").

start() ->
    console:puts("Opening socket ...\n"),
    Active = true,
    case ?GEN_UDP:open(44444, [{active, Active}]) of
        {ok, Socket} ->
            erlang:display(Socket),
            console:puts("Waiting to receive data...\n"),
            case Active of
                true -> active_loop();
                _ ->    passive_loop(Socket)
            end;
        ErrorReason ->
            erlang:display(ErrorReason)
    end.

active_loop() ->
    receive
        Msg -> erlang:display(Msg)
    end,
    active_loop().

passive_loop(Socket) ->
    case ?GEN_UDP:recv(Socket, 128) of
        {ok, RecvData} ->
            erlang:display(RecvData),
            passive_loop(Socket);
        ErrorReason ->
            erlang:display(ErrorReason)
    end.
