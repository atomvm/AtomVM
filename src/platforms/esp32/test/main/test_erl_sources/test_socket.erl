%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_socket).
-export([start/0]).

start() ->
    {ok, TCPClientAddr} = test_tcp_client(true, binary),
    {ok, TCPClientAddr} = test_tcp_client(true, list),
    {ok, TCPClientAddr} = test_tcp_client(false, binary),
    {ok, TCPClientAddr} = test_tcp_client(false, list),
    {ok, UDPAddr} = test_udp(true, 2023),
    {ok, UDPAddr} = test_udp(false, 2024),
    ok = test_tcp_server(true, 10080),
    ok = test_tcp_server(false, 10081),
    0.

test_tcp_client(Active, BinaryOpt) ->
    Socket = open_port({spawn, "socket"}, []),
    Params = [
        {proto, tcp},
        {connect, true},
        {controlling_process, self()},
        {address, "github.com"},
        {port, 80},
        {active, Active},
        {buffer, 512},
        {timeout, 30000},
        BinaryOpt
    ],
    ok = call(Socket, {init, Params}, 30000),
    {ok, {MyIPAddr, _Port}} = call(Socket, {sockname}),
    {ok, {_PeerIPAddr, 80}} = call(Socket, {peername}),
    ok =
        case call(Socket, {send, <<"GET / HTTP/1.0\r\n\r\n">>}) of
            % generic_unix socket driver
            {ok, _Len} -> ok;
            % esp32 socket driver
            ok -> ok;
            % any error
            SendError -> {send_error, SendError}
        end,
    ok =
        case Active of
            true ->
                ok =
                    receive
                        {tcp, _WrappedSocket, <<"HTTP/1.1 301", _/binary>>} when
                            BinaryOpt =:= binary
                        ->
                            ok;
                        {tcp, _WrappedSocket, "HTTP/1.1 301" ++ _} when BinaryOpt =:= list -> ok;
                        {tcp, _WrappedSocket, Packet} ->
                            {unexpected_packet, Packet, {Active, BinaryOpt}};
                        UnexpectedAfterSend ->
                            {test_tcp_client, unexpected_message_after_send, Socket,
                                UnexpectedAfterSend, {Active, BinaryOpt}}
                    after 10000 ->
                        {receive_packet, timeout}
                    end,
                ok =
                    receive
                        {tcp_closed, _OtherWrappedSocket} ->
                            ok;
                        UnexpectedAfterPacket ->
                            {unexpected_message_after_packet, Socket, UnexpectedAfterPacket,
                                {Active, BinaryOpt}}
                    after 10000 ->
                        {closed_socket, timeout}
                    end,
                % In current implementation, socket is closed and process is terminated
                % so we cannot close it.
                ok;
            false ->
                ok =
                    case call(Socket, {recv, 512, 30000}) of
                        {ok, <<"HTTP/1.1 301", _/binary>>} when BinaryOpt =:= binary -> ok;
                        {ok, "HTTP/1.1 301" ++ _} when BinaryOpt =:= list -> ok;
                        {ok, OtherData} -> {unexpected_packet, OtherData};
                        UnexpectedRecvResult -> {unexpected_recv_result, UnexpectedRecvResult}
                    end,
                ok =
                    case call(Socket, {close}) of
                        ok -> ok;
                        UnexpectedCloseResult -> {unexpected_close_result, UnexpectedCloseResult}
                    end,
                ok
        end,
    {ok, MyIPAddr}.

-define(UDP_QUERY(ID), <<
    ID:16,
    0:1,
    0:4,
    0:1,
    0:1,
    1:1,
    0:1,
    0:3,
    0:4,
    % QDCOUNT
    1:16,
    % ANCOUNT
    0:16,
    % NSCOUNT
    0:16,
    % ARCOUNT
    0:16,
    % QNAME
    6,
    "github",
    3,
    "com",
    0,
    % QTYPE
    1:16,
    % QCLASS
    1:16
>>).

test_udp(Active, QueryID) ->
    Socket = open_port({spawn, "socket"}, []),
    Params = [
        {proto, udp},
        {controlling_process, self()},
        {active, Active},
        {buffer, 512},
        {timeout, 30000},
        binary
    ],
    ok = call(Socket, {init, Params}, 30000),
    {ok, {MyIPAddr, _Port}} = call(Socket, {sockname}),
    ok =
        case call(Socket, {sendto, {1, 1, 1, 1}, 53, ?UDP_QUERY(QueryID)}) of
            % generic_unix socket driver
            {ok, _Len} -> ok;
            % esp32 socket driver
            ok -> ok;
            % any error
            SendError -> {sendto_error, SendError}
        end,
    ok =
        case Active of
            true ->
                ok =
                    receive
                        %               {udp, Socket, {{1,1,1,1}, 53, <<QueryID:16, 1:1, _:7, _/binary>>}} -> ok;    % not supported yet
                        {udp, _WrappedSocket, {1, 1, 1, 1}, 53, <<QueryID:16, B, _/binary>>} when
                            B band 16#80 =:= 16#80
                        ->
                            ok;
                        {udp, _WrappedSocket, Addr, Port, Packet} ->
                            {unexpected_packet, Addr, Port, Packet};
                        UnexpectedAfterSend ->
                            {test_udp, unexpected_message_after_send, UnexpectedAfterSend}
                    after 10000 ->
                        {receive_packet, timeout}
                    end,
                ok =
                    case call(Socket, {close}) of
                        ok -> ok;
                        UnexpectedCloseResult -> {unexpected_close_result, UnexpectedCloseResult}
                    end,
                ok;
            false ->
                ok =
                    case call(Socket, {recvfrom, 512, 30000}) of
                        %               {ok, {{1,1,1,1}, 53, <<QueryID:16, 1:1, _:7, _/binary>>}} -> ok;
                        {ok, {{1, 1, 1, 1}, 53, <<QueryID:16, B, _/binary>>}} when
                            B band 16#80 =:= 16#80
                        ->
                            ok;
                        {ok, OtherData} ->
                            {unexpected_packet, OtherData};
                        UnexpectedRecvResult ->
                            {unexpected_recv_result, UnexpectedRecvResult}
                    end,
                ok =
                    case call(Socket, {close}) of
                        ok -> ok;
                        UnexpectedCloseResult -> {unexpected_close_result, UnexpectedCloseResult}
                    end,
                ok
        end,
    {ok, MyIPAddr}.

test_tcp_server(Active, Port) ->
    ServerSocket = open_port({spawn, "socket"}, []),
    Params = [
        {proto, tcp},
        {listen, true},
        {controlling_process, self()},
        {backlog, 5},
        {port, Port},
        {active, Active},
        binary
    ],
    ok = call(ServerSocket, {init, Params}, 30000),
    Pid = spawn(fun() -> tcp_client(Active, Port) end),
    link(Pid),
    {ok, ConnectedSocket} =
        case call(ServerSocket, {accept, 30000}) of
            {ok, Socket} when is_pid(Socket) ->
                {ok, Socket};
            X ->
                {unexpected_accept, X}
        end,
    ok =
        case call(ConnectedSocket, {send, <<"ping">>}) of
            % generic_unix socket driver
            {ok, _Len} -> ok;
            % esp32 socket driver
            ok -> ok;
            % any error
            SendError -> {send_error, SendError}
        end,
    ok =
        case Active of
            true ->
                receive
                    {tcp, _WrappedConnectedSocket, <<"pong">>} ->
                        ok;
                    UnexpectedClientMessage ->
                        {test_tcp_server, unexpected_client, UnexpectedClientMessage}
                after 5000 -> client_timeout
                end;
            false ->
                case call(ConnectedSocket, {recv, 512, 30000}) of
                    {ok, <<"pong">>} -> ok;
                    {ok, OtherData} -> {unexpected_packet, OtherData};
                    UnexpectedRecvResult -> {unexpected_recv_result, UnexpectedRecvResult}
                end
        end,
    ok =
        case call(ConnectedSocket, {close}) of
            ok ->
                ok;
            UnexpectedClientCloseResult ->
                {unexpected_client_close_result, UnexpectedClientCloseResult}
        end,
    ok =
        case call(ServerSocket, {close}) of
            ok ->
                ok;
            UnexpectedServerCloseResult ->
                {unexpected_server_close_result, UnexpectedServerCloseResult}
        end,
    ok.

tcp_client(Active, Port) ->
    ClientSocket = open_port({spawn, "socket"}, []),
    Params = [
        {proto, tcp},
        {connect, true},
        {controlling_process, self()},
        {address, "127.0.0.1"},
        {port, Port},
        {active, Active},
        {buffer, 512},
        {timeout, 30000},
        binary
    ],
    ok = call(ClientSocket, {init, Params}, 30000),
    ok =
        case Active of
            true ->
                receive
                    {tcp, _WrappedClientSocketPing, <<"ping">>} ->
                        ok;
                    UnexpectedClientMessage ->
                        {tcp_client, unexpected_client, UnexpectedClientMessage}
                after 5000 -> client_timeout
                end;
            false ->
                case call(ClientSocket, {recv, 512, 30000}) of
                    {ok, <<"ping">>} -> ok;
                    {ok, OtherData} -> {unexpected_packet, OtherData};
                    UnexpectedRecvResult -> {unexpected_recv_result, UnexpectedRecvResult}
                end
        end,
    ok =
        case call(ClientSocket, {send, <<"pong">>}) of
            % generic_unix socket driver
            {ok, _Len} -> ok;
            % esp32 socket driver
            ok -> ok;
            % any error
            SendError -> {send_error, SendError}
        end,
    ok =
        case Active of
            true ->
                receive
                    {tcp_closed, _WrappedClientSocketClosed} ->
                        ok;
                    UnexpectedAfterPacket ->
                        {unexpected_message_after_pong, ClientSocket, UnexpectedAfterPacket}
                after 10000 ->
                    {closed_socket, timeout}
                end;
            % In current implementation, socket is closed and process is terminated
            % so we cannot close it.
            false ->
                ok =
                    case call(ClientSocket, {close}) of
                        ok -> ok;
                        UnexpectedCloseResult -> {unexpected_close_result, UnexpectedCloseResult}
                    end
        end,
    ok.

call(DriverPid, Msg) ->
    call(DriverPid, Msg, 5000).

call(DriverPid, Msg, Timeout) ->
    Ref = erlang:make_ref(),
    DriverPid ! {self(), Ref, Msg},
    receive
        {Ref, Ret} ->
            Ret
    after Timeout ->
        {driver_call, Msg, timeout}
    end.
