%
% This file is part of AtomVM.
%
% Copyright 2021 Davide Bettio <davide@uninstall.it>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(network_console).

-export([start/0, listen/1, start_repl/1]).

-record(nc_state, {socket, pending_pid, pending_ref}).

start() ->
    listen(2323),
    sleep_forever().

listen(Port) ->
    case gen_tcp:listen(Port, []) of
        {ok, ListenSocket} ->
            io:format("Listening on ~s.~n", [local_address(ListenSocket)]),
            spawn(fun() -> accept(ListenSocket) end);
        Error ->
            io:format("An error occurred listening: ~p~n", [Error])
    end.

accept(ListenSocket) ->
    io:format("Waiting to accept shell connection...~n"),
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn_opt(?MODULE, start_repl, [self()], [link]),
            io:format("Accepted shell connection. local: ~s peer: ~s~n", [
                local_address(Socket), peer_address(Socket)
            ]),
            spawn(fun() -> accept(ListenSocket) end),
            loop(#nc_state{socket = Socket});
        Error ->
            io:format("An error occurred accepting connection: ~p~n", [Error])
    end.

loop(State) ->
    receive
        {tcp_closed, _Socket} ->
            io:format("Connection closed.~n"),
            erlang:exit(connection_closed);
        {tcp, _Socket, <<255, 244, 255, 253, 6>>} ->
            io:format("Break.~n"),
            gen_tcp:close(State#nc_state.socket),
            erlang:exit(break);
        {tcp, _Socket, Packet} ->
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

to_string({{A, B, C, D}, Port}) ->
    io_lib:format("~p.~p.~p.~p:~p", [A, B, C, D, Port]).

sleep_forever() ->
    timer:sleep(10000),
    sleep_forever().

start_repl(SocketIOLeader) ->
    erlang:group_leader(SocketIOLeader, self()),
    arepl:start().

io_request({get_line, unicode, Data}, FPid, FRef, State) ->
    gen_tcp:send(State#nc_state.socket, Data),
    {ok, State#nc_state{pending_pid = FPid, pending_ref = FRef}};
io_request({put_chars, unicode, Data}, FPid, FRef, State) ->
    gen_tcp:send(State#nc_state.socket, Data),
    FPid ! {io_reply, FRef, ok},
    {ok, State}.
