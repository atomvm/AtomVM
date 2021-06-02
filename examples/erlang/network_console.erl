-module(network_console).

-export([start/0]).

-record(nc_state, {socket, pending_pid, pending_ref}).

start() ->
    case gen_tcp:listen(2323, []) of
        {ok, ListenSocket} ->
            io:format("Listening on ~p.~n", [local_address(ListenSocket)]),
            spawn(fun() -> accept(ListenSocket) end),
            sleep_forever();
        Error ->
            io:format("An error occurred listening: ~p~n", [Error])
    end.

accept(ListenSocket) ->
    io:format("Waiting to accept shell connection...~n"),
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            SocketIOLeader = self(),
            spawn(fun() ->
                erlang:group_leader(SocketIOLeader, self()),
                arepl:start()
            end),
            io:format("Accepted shell connection. local: ~p peer: ~p~n", [local_address(Socket), peer_address(Socket)]),
            spawn(fun() -> accept(ListenSocket) end),
            loop(#nc_state{socket = Socket});
        Error ->
            io:format("An error occurred accepting connection: ~p~n", [Error])
    end.

loop(State) ->
    io:format("Waiting to receive data...~n"),
    receive
        {tcp_closed, _Socket} ->
            io:format("Connection closed.~n"),
            ok;
        {tcp, Socket, Packet} ->
            erlang:display({got, Packet}),
            Reply = {io_reply, State#nc_state.pending_ref, Packet},
            State#nc_state.pending_pid ! Reply,
            loop(State#nc_state{pending_pid = undefined, pending_ref = undefined});

        {io_request, FPid, FRef, Request} ->
            {ok, NewState} = io_request(Request, FPid, FRef, State),
            loop(NewState)
    end.

local_address(Socket) ->
    {ok, SockName} = inet:sockname(Socket),
    to_string(SockName).

peer_address(Socket) ->
    {ok, Peername} = inet:peername(Socket),
    to_string(Peername).

to_string({{A,B,C,D}, Port}) ->
    io_lib:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]).

sleep_forever() ->
    timer:sleep(10000),
    sleep_forever().

io_request({get_line, unicode, Data}, FPid, FRef, State) ->
    gen_tcp:send(State#nc_state.socket, Data),
    {ok, State#nc_state{pending_pid = FPid, pending_ref = FRef}};

io_request({put_chars, unicode, Data}, FPid, FRef, State) ->
    gen_tcp:send(State#nc_state.socket, Data),
    FPid ! {io_reply, FRef, ok},
    {ok, State}.
