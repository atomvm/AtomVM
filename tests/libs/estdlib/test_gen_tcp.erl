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
    spawn(
        fun() ->
            Self ! ready,
            accept(Self, ListenSocket, SpawnControllingProcess)
        end
    ),
    receive
        ready ->
            ok
    end,

    test_send_receive(Port, 10, SpawnControllingProcess),

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
            echo(Pid, Socket);
        SomethingElse ->
            erlang:display({echo, unexpected_message, SomethingElse})
    end.

test_send_receive(Port, N, SpawnControllingProcess) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [{active, true}]),
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
    case get_otp_version() of
        Version when Version =:= atomvm orelse (is_integer(Version) andalso Version >= 24) ->
            ok = test_listen_connect_parameters(socket, socket),
            ok = test_listen_connect_parameters(inet, inet);
        _ ->
            ok = test_listen_connect_parameters(inet, inet)
    end,
    ok.

test_listen_connect_parameters(InetClientBackend, InetServerBackend) ->
    Results = [
        test_listen_connect_parameters(
            InetClientBackend,
            InetServerBackend,
            ListenMode,
            ConnectMode,
            ListenActive,
            ConnectActive
        )
     || ListenMode <- [binary, list],
        ConnectMode <- [binary, list],
        ListenActive <- [false, true],
        ConnectActive <- [false, true]
    ],
    [] = [Error || Error <- Results, Error =/= ok],
    ok.

test_listen_connect_parameters(
    InetClientBackend, InetServerBackend, ListenMode, ConnectMode, ListenActive, ConnectActive
) ->
    etest:flush_msg_queue(),

    case get_otp_version() of
        Version when Version =:= atomvm orelse (is_integer(Version) andalso Version >= 24) ->
            ServerBackendOption = [{inet_backend, InetServerBackend}],
            ClientBackendOption = [{inet_backend, InetClientBackend}];
        _ ->
            ServerBackendOption = [],
            ClientBackendOption = []
    end,

    io:format(
        "GEN_TCP-TEST> ServerBackendOption=~p ClientBackendOption=~p ListenMode=~p ConnectMode=~p ListenActive=~p ConnectActive=~p~n",
        [
            ServerBackendOption,
            ClientBackendOption,
            ListenMode,
            ConnectMode,
            ListenActive,
            ConnectActive
        ]
    ),

    NumMessages = 10,

    {ok, ListenSocket} = gen_tcp:listen(
        0,
        ServerBackendOption ++ [ListenMode, {active, ListenActive}, {buffer, 32}]
    ),
    {ok, {_Address, Port}} = inet:sockname(ListenSocket),

    Self = self(),
    ServerPid = spawn(fun() ->
        Self ! {self(), ready},
        Result = test_listen_connect_parameters_accept(
            ListenMode, ListenActive, ListenSocket, NumMessages, Self
        ),
        Self ! {self(), Result}
    end),
    receive
        {ServerPid, ready} ->
            ok
    end,

    {ok, Socket} = gen_tcp:connect(
        {127, 0, 0, 1},
        Port,
        ClientBackendOption ++ [ConnectMode, {active, ConnectActive}]
    ),
    ok = test_listen_connect_parameters_client_loop(
        Socket, ConnectMode, ConnectActive, NumMessages
    ),

    %% race condition in active receive; client might
    %% close connection before service has consumed (and delivered)
    %% all messages to active recipient process.  So we need
    %% to wait until the server has actually processed all the
    %% messages it is expected to.
    receive
        server_done ->
            ok
    end,

    ok = gen_tcp:close(Socket),

    receive
        {ServerPid, Result} ->
            ok = gen_tcp:close(ListenSocket),
            Result
    after 5000 ->
        throw({timeout, waiting, recv, server_closed})
    end.

test_listen_connect_parameters_client_loop(_Socket, _Mode, _Active, 0) ->
    ok;
test_listen_connect_parameters_client_loop(Socket, Mode, Active, I) ->
    Packet = list_to_binary(pid_to_list(self()) ++ ":" ++ integer_to_list(I)),
    ok = gen_tcp:send(Socket, Packet),
    test_listen_connect_parameters_client_loop0(Socket, Mode, Active, I).

test_listen_connect_parameters_client_loop0(Socket, Mode, true = Active, I) ->
    receive
        {tcp_closed, _Socket} ->
            ok;
        {tcp, _Socket, Packet} ->
            if
                Mode =:= binary andalso is_binary(Packet) ->
                    test_listen_connect_parameters_client_loop(Socket, Mode, Active, I - 1);
                Mode =:= list andalso is_list(Packet) ->
                    test_listen_connect_parameters_client_loop(Socket, Mode, Active, I - 1);
                true ->
                    {error,
                        {unexpected_packet_format, client, active_receive, Packet, Mode, Active}}
            end;
        {error, _Reason} = Error ->
            {error, {unexpected_message, client, active_receive, Error}}
    after 5000 ->
        {error, {timeout, client, active_receive, Mode, I}}
    end;
test_listen_connect_parameters_client_loop0(Socket, Mode, false = Active, I) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {error, closed} ->
            ok;
        {ok, Packet} ->
            if
                Mode =:= binary andalso is_binary(Packet) ->
                    test_listen_connect_parameters_client_loop(Socket, Mode, Active, I - 1);
                Mode =:= list andalso is_list(Packet) ->
                    test_listen_connect_parameters_client_loop(Socket, Mode, Active, I - 1);
                true ->
                    {error, {unexpected_packet_format, client, passive_receive, Packet, Mode}}
            end;
        Other ->
            {error, {unexpected_result, client, passive_receive, Other}}
    end.

test_listen_connect_parameters_accept(
    ListenMode, ListenActive, ListenSocket, NumMessages, WaitingPid
) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    try
        test_listen_connect_parameters_server_loop(
            ListenMode, ListenActive, Socket, NumMessages, WaitingPid
        )
    after
        ok = gen_tcp:close(Socket)
    end.

test_listen_connect_parameters_server_loop(
    _ListenMode, true = _ListenActive, Socket, 0, WaitingPid
) ->
    WaitingPid ! server_done,
    receive
        {tcp_closed, Socket} ->
            ok
    after 5000 ->
        {error, {timeout, server, active_receive, waiting_for_close}}
    end;
test_listen_connect_parameters_server_loop(ListenMode, true = ListenActive, Socket, I, WaitingPid) ->
    receive
        {tcp_closed, _Socket} ->
            {error, {unexpected_close, server, active_receive}};
        {tcp, Socket, Packet} ->
            ok = gen_tcp:send(Socket, Packet),
            if
                ListenMode =:= binary andalso is_binary(Packet) ->
                    test_listen_connect_parameters_server_loop(
                        ListenMode, ListenActive, Socket, I - 1, WaitingPid
                    );
                ListenMode =:= list andalso is_list(Packet) ->
                    test_listen_connect_parameters_server_loop(
                        ListenMode, ListenActive, Socket, I - 1, WaitingPid
                    );
                true ->
                    {error, {unexpected_packet_format, server, active_receive, Packet, ListenMode}}
            end;
        Other ->
            {error, {unexpected_message, server, active_receive, Other}}
    after 5000 ->
        {error, {timeout, server, active_receive, ListenMode}}
    end;
test_listen_connect_parameters_server_loop(
    _ListenMode, false = _ListenActive, Socket, 0, WaitingPid
) ->
    WaitingPid ! server_done,
    case gen_tcp:recv(Socket, 0, 5000) of
        {error, closed} ->
            ok;
        {error, timeout} ->
            {error, {timeout, server, passive_receive, waiting_for_close}}
    end;
test_listen_connect_parameters_server_loop(ListenMode, false = ListenActive, Socket, I, WaitingPid) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {error, closed} ->
            {error, {unexpected_close, server, passive_receive}};
        {ok, Packet} ->
            ok = gen_tcp:send(Socket, Packet),
            if
                ListenMode =:= binary andalso is_binary(Packet) ->
                    test_listen_connect_parameters_server_loop(
                        ListenMode, ListenActive, Socket, I - 1, WaitingPid
                    );
                ListenMode =:= list andalso is_list(Packet) ->
                    test_listen_connect_parameters_server_loop(
                        ListenMode, ListenActive, Socket, I - 1, WaitingPid
                    );
                true ->
                    {error, {unexpected_packet_format, server, passive_receive, Packet, ListenMode}}
            end;
        Other ->
            {error, {unexpected_result, server, passive_receive, Other}}
    end.

test_tcp_double_close() ->
    {ok, Socket} = gen_tcp:listen(10543, [{active, false}]),
    ok = gen_tcp:close(Socket),
    ok = gen_tcp:close(Socket),
    {error, closed} = gen_tcp:recv(Socket, 512, 5000),
    ok.

get_otp_version() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            list_to_integer(erlang:system_info(otp_release));
        _ ->
            atomvm
    end.
