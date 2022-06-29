%
% This file is part of AtomVM.
%
% Copyright 2019-2022 Fred Dushin <fred@dushin.net>
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

-module(test_tcp_socket).

-export([test/0]).

test() ->
    ok = test_echo_server(),
    ok.

test_echo_server() ->
    Port = 44404,
    {ok, ListenSocket} = socket:open(inet, stream, tcp),
    case erlang:system_info(machine) of
        "BEAM" ->
            OTPRelease = list_to_integer(erlang:system_info(otp_release)),
            case OTPRelease >= 24 of
                true ->
                    ok = socket:setopt(ListenSocket, {socket, reuseaddr}, true),
                    ok = socket:setopt(ListenSocket, {socket, linger}, #{onoff => true, linger => 0});
                _ ->
                    ok
            end;
        _ ->
            ok = socket:setopt(ListenSocket, {socket, reuseaddr}, true),
            ok = socket:setopt(ListenSocket, {socket, linger}, #{onoff => true, linger => 0})
    end,

    %% TODO OTP/22 returns {ok, Port}, which is not consistent with API docs; should return ok
    socket:bind(ListenSocket, #{family => inet, addr => loopback, port => Port}),
    ok = socket:listen(ListenSocket),

    Self = self(),
    spawn(fun() ->
        Self ! ready,
        accept(Self, ListenSocket)
    end),

    erlang:display(waiting_for_ready),
    receive
        ready ->
            ok
    end,

    test_send_receive(Port, 10),

    ok = socket:close(ListenSocket),

    ok.

accept(Pid, ListenSocket) ->
    case socket:accept(ListenSocket) of
        {ok, Socket} ->
            erlang:display({accepted, Socket}),
            spawn(fun() -> accept(Pid, ListenSocket) end),
            echo(Pid, Socket);
        {error, closed} ->
            ok
    end.

echo(Pid, Socket) ->
    case socket:recv(Socket) of
        {ok, Packet} ->
            erlang:display({server, recvd, Packet}),
            ok =
                case socket:send(Socket, Packet) of
                    ok ->
                        ok;
                    {ok, []} ->
                        ok;
                    E ->
                        {error, E}
                end,
            erlang:display({server, sent, Packet}),
            echo(Pid, Socket);
        {error, closed} ->
            erlang:display({server, closed}),
            % Pid ! server_closed,
            ok;
        Error ->
            erlang:display({server, recv, err, Error}),
            Pid ! server_closed,
            ok
    end.

test_send_receive(Port, N) ->
    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = socket:connect(Socket, #{family => inet, addr => loopback, port => Port}),

    timer:sleep(100),

    ok = loop(Socket, N),

    %% TODO: closing the client connection is not notifying server, for some reason
    % erlang:display(waiting_for_server_closed),
    % receive
    %     server_closed -> ok
    % after 1000 -> throw({timeout, waiting, recv, server_closed})
    % end.
    ok.

loop(Socket, 0) ->
    erlang:display(closing_client_socket),
    ok = socket:close(Socket),
    erlang:display(done),
    ok;
loop(Socket, I) ->
    Packet = list_to_binary(pid_to_list(self()) ++ ":" ++ integer_to_list(I)),
    case socket:send(Socket, Packet) of
        ok ->
            erlang:display({client, sent, Packet}),
            case socket:recv(Socket) of
                {ok, OtherPacket} ->
                    erlang:display({client, recvd, OtherPacket}),
                    loop(Socket, I - 1);
                Error ->
                    io:format("Error on recv: ~p~n", [Error])
            end;
        {ok, []} ->
            erlang:display({client, sent, Packet}),
            case socket:recv(Socket) of
                {ok, OtherPacket} ->
                    erlang:display({client, recvd, OtherPacket}),
                    loop(Socket, I - 1);
                Error ->
                    io:format("Error on recv: ~p~n", [Error])
            end;
        Error ->
            io:format("Error on send: ~p~n", [Error]),
            Error
    end.
