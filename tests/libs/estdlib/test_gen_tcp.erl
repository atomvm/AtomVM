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

-module(test_gen_tcp).

-export([test/0]).

-include("etest.hrl").

test() ->
    ok = test_echo_server(),
    ok = test_echo_server(true),
    ok = test_listen_connect_parameters(),
    ok = test_tcp_double_close(),
    ok.

test_echo_server() ->
    test_echo_server(false).

test_echo_server(SpawnControllingProcess) ->
    {ok, ListenSocket} = gen_tcp:listen(0, []),
    {ok, {_Address, Port}} = inet:sockname(ListenSocket),

    Self = self(),
    spawn(fun() ->
        Self ! ready,
        accept(Self, ListenSocket, SpawnControllingProcess)
    end),
    receive
        ready ->
            ok
    end,

    test_send_receive(Port, 100, SpawnControllingProcess),

    %% TODO bug closing listening socket
    % gen_tcp:close(ListenSocket),

    ok.

accept(Pid, ListenSocket, SpawnControllingProcess) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> accept(Pid, ListenSocket, SpawnControllingProcess) end),
            case SpawnControllingProcess of
                false ->
                    echo(Pid, Socket);
                true ->
                    NewPid = spawn(fun() -> echo(Pid, Socket) end),
                    ok = gen_tcp:controlling_process(Socket, NewPid)
            end;
        {error, closed} ->
            ok
    end.

echo(Pid, Socket) ->
    receive
        {tcp_closed, _Socket} ->
            Pid ! server_closed,
            ok;
        {tcp, Socket, Packet} ->
            ok = gen_tcp:send(Socket, Packet),
            echo(Pid, Socket)
    end.

test_send_receive(Port, N, SpawnControllingProcess) ->
    {ok, Socket} = gen_tcp:connect(localhost, Port, [{active, true}]),
    case SpawnControllingProcess of
        false ->
            loop(Socket, N);
        true ->
            Pid = spawn(fun() ->
                receive
                    {Parent, go} ->
                        loop(Socket, N),
                        Parent ! done
                end
            end),
            gen_tcp:controlling_process(Socket, Pid),
            Pid ! {self(), go},
            receive
                done -> ok
            end
    end,

    gen_tcp:close(Socket),
    receive
        server_closed -> ok
    after 1000 -> throw({timeout, waiting, recv, server_closed})
    end.

loop(_Socket, 0) ->
    ok;
loop(Socket, I) ->
    Packet = list_to_binary(pid_to_list(self()) ++ ":" ++ integer_to_list(I)),
    ok = gen_tcp:send(Socket, Packet),
    receive
        {tcp_closed, _OtherSocket} ->
            ok;
        {tcp, _OtherSocket, _OtherPacket} ->
            loop(Socket, I - 1)
    end.

test_listen_connect_parameters() ->
    Results = [
        test_listen_connect_parameters(ListenMode, ConnectMode, ListenActive, ConnectActive)
     || ListenMode <- [binary, list],
        ConnectMode <- [binary, list],
        ListenActive <- [false, true],
        ConnectActive <- [false, true]
    ],
    [] = [Error || Error <- Results, Error =/= ok],
    ok.

test_listen_connect_parameters(ListenMode, ConnectMode, ListenActive, ConnectActive) ->
    {ok, ListenSocket} = gen_tcp:listen(0, [ListenMode, {active, ListenActive}, {buffer, 32}]),
    {ok, {_Address, Port}} = inet:sockname(ListenSocket),

    Self = self(),
    ServerPid = spawn(fun() ->
        Self ! {self(), ready},
        Result = test_listen_connect_parameters_accept(ListenMode, ListenActive, ListenSocket),
        Self ! {self(), Result}
    end),
    receive
        {ServerPid, ready} ->
            ok
    end,

    {ok, Socket} = gen_tcp:connect(localhost, Port, [ConnectMode, {active, ConnectActive}]),
    ok = test_listen_connect_parameters_client_loop(Socket, ConnectMode, ConnectActive, 10),
    ok = gen_tcp:close(Socket),
    receive
        {ServerPid, Result} -> Result
    after 5000 -> throw({timeout, waiting, recv, server_closed})
    end.

test_listen_connect_parameters_client_loop(_Socket, _Mode, _Active, 0) ->
    ok;
test_listen_connect_parameters_client_loop(Socket, Mode, Active, I) ->
    Packet = list_to_binary(pid_to_list(self()) ++ ":" ++ integer_to_list(I)),
    ok = gen_tcp:send(Socket, Packet),
    test_listen_connect_parameters_client_loop0(Socket, Mode, Active, I).

test_listen_connect_parameters_client_loop0(Socket, Mode, true = Active, I) ->
    receive
        {tcp_closed, Socket} ->
            ok;
        {tcp, Socket, Packet} ->
            if
                Mode =:= binary andalso is_binary(Packet) ->
                    test_listen_connect_parameters_client_loop(Socket, Mode, Active, I - 1);
                Mode =:= list andalso is_list(Packet) ->
                    test_listen_connect_parameters_client_loop(Socket, Mode, Active, I - 1);
                true ->
                    {error, {unexpected_packet_format, client, Packet, Mode}}
            end;
        Other ->
            {error, {unexpected_message, Other}}
    end;
test_listen_connect_parameters_client_loop0(Socket, Mode, false = Active, I) ->
    case gen_tcp:recv(Socket, 0) of
        {error, closed} ->
            ok;
        {ok, Packet} ->
            if
                Mode =:= binary andalso is_binary(Packet) ->
                    test_listen_connect_parameters_client_loop(Socket, Mode, Active, I - 1);
                Mode =:= list andalso is_list(Packet) ->
                    test_listen_connect_parameters_client_loop(Socket, Mode, Active, I - 1);
                true ->
                    {error, {unexpected_packet_format, client, Packet, Mode}}
            end;
        Other ->
            {error, {unexpected_result, client, Other}}
    end.

test_listen_connect_parameters_accept(ListenMode, ListenActive, ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    test_listen_connect_parameters_server_loop(ListenMode, ListenActive, Socket).

test_listen_connect_parameters_server_loop(ListenMode, true = ListenActive, Socket) ->
    receive
        {tcp_closed, Socket} ->
            ok;
        {tcp, Socket, Packet} ->
            ok = gen_tcp:send(Socket, Packet),
            if
                ListenMode =:= binary andalso is_binary(Packet) ->
                    test_listen_connect_parameters_server_loop(ListenMode, ListenActive, Socket);
                ListenMode =:= list andalso is_list(Packet) ->
                    test_listen_connect_parameters_server_loop(ListenMode, ListenActive, Socket);
                true ->
                    {error, {unexpected_packet_format, server, Packet, ListenMode}}
            end;
        Other ->
            {error, {unexpected_message, Other}}
    end;
test_listen_connect_parameters_server_loop(ListenMode, false = ListenActive, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {error, closed} ->
            ok;
        {ok, Packet} ->
            ok = gen_tcp:send(Socket, Packet),
            if
                ListenMode =:= binary andalso is_binary(Packet) ->
                    test_listen_connect_parameters_server_loop(ListenMode, ListenActive, Socket);
                ListenMode =:= list andalso is_list(Packet) ->
                    test_listen_connect_parameters_server_loop(ListenMode, ListenActive, Socket);
                true ->
                    {error, {unexpected_packet_format, server, Packet, ListenMode}}
            end;
        Other ->
            {error, {unexpected_result, server, Other}}
    end.

test_tcp_double_close() ->
    {ok, Socket} = gen_tcp:listen(10543, [{active, false}]),
    ok = gen_tcp:close(Socket),
    ok = gen_tcp:close(Socket),
    {error, closed} = gen_tcp:recv(Socket, 512, 5000),
    ok.
