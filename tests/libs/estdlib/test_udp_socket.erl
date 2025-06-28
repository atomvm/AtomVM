%
% This file is part of AtomVM.
%
% Copyright 2023 Fred Dushin <fred@dushin.net>
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

-module(test_udp_socket).

-export([test/0]).

test() ->
    ok = test_echo(),
    ok = test_buf_size(),
    ok = test_timeout(),
    ok = test_nowait(),
    ok = test_setopt_getopt(),
    % Workaround for image limitation on CI
    % https://github.com/actions/runner-images/issues/10924
    Platform = erlang:system_info(machine),
    System = execute_command(Platform, "uname -s"),
    TestMulticast =
        case System of
            "Darwin\n" ->
                Version = execute_command(Platform, "uname -r"),
                Version < "24";
            _ ->
                true
        end,
    if
        TestMulticast ->
            ok = test_multicast();
        true ->
            ok
    end,
    ok.

-define(PACKET_SIZE, 7).

start_echo_server(Port) ->
    {ok, Socket} = socket:open(inet, dgram, udp),

    ok = socket:setopt(Socket, {socket, reuseaddr}, true),
    ok = socket:setopt(Socket, {socket, linger}, #{onoff => true, linger => 0}),

    ok = socket:bind(Socket, #{
        family => inet, addr => loopback, port => Port
    }),

    {Pid, MonitorRef} = spawn_opt(
        fun() ->
            echo_server_loop(Socket)
        end,
        [monitor]
    ),

    {Pid, MonitorRef, Socket}.

echo_server_loop(Socket) ->
    case socket:recvfrom(Socket, 0, 5000) of
        {ok, {Source, <<"echo:", _/binary>> = Packet}} ->
            ok = socket:sendto(Socket, Packet, Source),
            echo_server_loop(Socket);
        {ok, {Source, <<"wait:", _/binary>> = Packet}} ->
            timer:sleep(500),
            ok = socket:sendto(Socket, Packet, Source),
            echo_server_loop(Socket);
        {ok, {Source, <<"chnk:", Rest/binary>>}} ->
            ok = socket:sendto(Socket, <<"chnk:">>, Source),
            ok = socket:sendto(Socket, Rest, Source),
            echo_server_loop(Socket);
        {error, closed} ->
            ok;
        SomethingElse ->
            error({unexpected_return_from_recv, SomethingElse})
    end.

stop_echo_server({Pid, MonitorRef, Socket}) ->
    % We stop the server by closing the packet.
    ok = socket:close(Socket),
    normal =
        receive
            {'DOWN', MonitorRef, process, Pid, Reason} -> Reason
        end,
    ok.

test_echo() ->
    Port = 44405,
    EchoServer = start_echo_server(Port),
    Dest = #{family => inet, addr => {127, 0, 0, 1}, port => Port},
    {ok, Socket} = socket:open(inet, dgram, udp),

    % Test recvfrom
    ok = socket:sendto(Socket, <<"echo:01">>, Dest),
    {ok, {Dest, <<"echo:01">>}} = socket:recvfrom(Socket, 0, 5000),

    % Test recv
    ok = socket:sendto(Socket, <<"echo:02">>, Dest),
    {ok, <<"echo:02">>} = socket:recv(Socket, 0, 5000),

    % Test loopback
    ok = socket:sendto(Socket, <<"echo:03">>, #{family => inet, addr => loopback, port => Port}),
    {ok, {Dest, <<"echo:03">>}} = socket:recvfrom(Socket, 0, 5000),

    % Chunk means two packets with UDP
    ok = socket:sendto(Socket, <<"chnk:01">>, Dest),
    timer:sleep(200),
    {ok, {Dest, <<"chnk:">>}} = socket:recvfrom(Socket, 0, 5000),
    {ok, {Dest, <<"01">>}} = socket:recvfrom(Socket, 0, 5000),

    % Chunk means two packets with UDP, including with recv
    ok = socket:sendto(Socket, <<"chnk:02">>, Dest),
    timer:sleep(200),
    {ok, <<"chnk:">>} = socket:recv(Socket, 0, 5000),
    {ok, <<"02">>} = socket:recv(Socket, 0, 5000),

    ok = socket:close(Socket),
    ok = stop_echo_server(EchoServer).

test_buf_size() ->
    Port = 44405,
    EchoServer = start_echo_server(Port),
    Dest = #{family => inet, addr => {127, 0, 0, 1}, port => Port},
    {ok, Socket} = socket:open(inet, dgram, udp),

    %% try a few failures first
    {error, _} = socket:setopt(Socket, {otp, badopt}, any_value),
    {error, _} = socket:setopt(Socket, {otp, rcvbuf}, not_an_int),
    {error, _} = socket:setopt(Socket, {otp, rcvbuf}, -1),

    %% limit the recv buffer size to 5 bytes
    ok = socket:setopt(Socket, {otp, rcvbuf}, 5),
    true = 5 < ?PACKET_SIZE,

    %% we should only be able to receive
    ok = socket:sendto(Socket, <<"echo:01">>, Dest),
    {ok, {Dest, <<"echo:">>}} = socket:recvfrom(Socket, 0, 5000),
    {error, timeout} = socket:recvfrom(Socket, 0, 0),
    ok = socket:sendto(Socket, <<"echo:01">>, Dest),
    {ok, {Dest, <<"echo:">>}} = socket:recvfrom(Socket, 0, 5000),
    {error, timeout} = socket:recvfrom(Socket, 0, 0),

    %% verify that the socket:recv length parameter takes
    %% precedence over the default
    ok = socket:sendto(Socket, <<"echo:03">>, Dest),
    {ok, {Dest, <<"echo:03">>}} = socket:recvfrom(Socket, ?PACKET_SIZE, 5000),

    ok = socket:close(Socket),
    ok = stop_echo_server(EchoServer).

test_timeout() ->
    Port = 44405,
    EchoServer = start_echo_server(Port),
    Dest = #{family => inet, addr => {127, 0, 0, 1}, port => Port},
    {ok, Socket} = socket:open(inet, dgram, udp),

    % Test recvfrom
    ok = socket:sendto(Socket, <<"wait:01">>, Dest),
    {error, timeout} = socket:recvfrom(Socket, 0, 100),
    {ok, {Dest, <<"wait:01">>}} = socket:recvfrom(Socket, 0, 5000),

    ok = socket:sendto(Socket, <<"wait:02">>, Dest),
    {error, timeout} = socket:recvfrom(Socket, ?PACKET_SIZE, 0),
    {ok, {Dest, <<"wait:02">>}} = socket:recvfrom(Socket, ?PACKET_SIZE, 5000),

    ok = socket:sendto(Socket, <<"wait:03">>, Dest),
    {error, timeout} = socket:recvfrom(Socket, 0, 0),
    {ok, {Dest, <<"wait:03">>}} = socket:recvfrom(Socket, 10, infinity),

    % Test recv
    ok = socket:sendto(Socket, <<"wait:01">>, Dest),
    {error, timeout} = socket:recv(Socket, 0, 100),
    {ok, <<"wait:01">>} = socket:recv(Socket, 0, 5000),

    ok = socket:sendto(Socket, <<"wait:02">>, Dest),
    {error, timeout} = socket:recv(Socket, ?PACKET_SIZE, 0),
    {ok, <<"wait:02">>} = socket:recv(Socket, ?PACKET_SIZE, 5000),

    ok = socket:sendto(Socket, <<"wait:03">>, Dest),
    {error, timeout} = socket:recv(Socket, 2 * ?PACKET_SIZE, 0),
    ok =
        case socket:recv(Socket, 2 * ?PACKET_SIZE, 5000) of
            {ok, <<"wait:03">>} ->
                ok;
            % https://github.com/erlang/otp/issues/9172
            {error, {timeout, <<"wait:03">>}} ->
                "BEAM" = erlang:system_info(machine),
                case erlang:system_info(otp_release) of
                    "26" -> ok;
                    "27" -> ok
                end,
                ok
        end,

    ok = socket:close(Socket),
    ok = stop_echo_server(EchoServer).

test_nowait() ->
    ok = test_nowait(fun receive_loop_nowait/2),
    ok = test_nowait(fun receive_loop_nowait_ref/2),
    ok = test_nowait(fun receive_loop_recvfrom_nowait/2),
    ok = test_nowait(fun receive_loop_recvfrom_nowait_ref/2),
    ok.

test_nowait(ReceiveFun) ->
    etest:flush_msg_queue(),

    Port = 44404,
    EchoServer = start_echo_server(Port),
    Dest = #{family => inet, addr => {127, 0, 0, 1}, port => Port},
    {ok, Socket} = socket:open(inet, dgram, udp),

    Packet0 = <<"echo:00">>,
    ok = socket:sendto(Socket, Packet0, Dest),
    ok = ReceiveFun(Socket, Packet0),

    Packet1 = <<"wait:00">>,
    ok = socket:sendto(Socket, Packet1, Dest),
    ok = ReceiveFun(Socket, Packet1),

    ok = socket:close(Socket),
    ok = stop_echo_server(EchoServer).

receive_loop_nowait(Socket, Packet) ->
    case socket:recv(Socket, byte_size(Packet), nowait) of
        {ok, ReceivedPacket} when ReceivedPacket =:= Packet ->
            ok;
        {select, {select_info, recv, SelectHandle}} when is_reference(SelectHandle) ->
            receive
                {'$socket', Socket, select, SelectHandle} ->
                    receive_loop_nowait(Socket, Packet)
            after 5000 ->
                {error, timeout}
            end;
        {error, _} = Error ->
            io:format("Error on recv: ~p~n", [Error]),
            Error
    end.

receive_loop_nowait_ref(Socket, Packet) ->
    Ref = make_ref(),
    case socket:recv(Socket, byte_size(Packet), Ref) of
        {ok, ReceivedPacket} when ReceivedPacket =:= Packet ->
            ok;
        {select, {select_info, recv, Ref}} ->
            receive
                {'$socket', Socket, select, Ref} ->
                    receive_loop_nowait_ref(Socket, Packet)
            after 5000 ->
                {error, timeout}
            end;
        {error, _} = Error ->
            io:format("Error on recv: ~p~n", [Error]),
            Error
    end.

receive_loop_recvfrom_nowait(Socket, Packet) ->
    case socket:recvfrom(Socket, byte_size(Packet), nowait) of
        {ok, {_Source, ReceivedPacket}} when ReceivedPacket =:= Packet ->
            ok;
        {select, {select_info, recvfrom, SelectHandle}} when is_reference(SelectHandle) ->
            receive
                {'$socket', Socket, select, SelectHandle} ->
                    receive_loop_nowait(Socket, Packet)
            after 5000 ->
                {error, timeout}
            end;
        {error, _} = Error ->
            io:format("Error on recv: ~p~n", [Error]),
            Error
    end.

receive_loop_recvfrom_nowait_ref(Socket, Packet) ->
    Ref = make_ref(),
    case socket:recvfrom(Socket, byte_size(Packet), Ref) of
        {ok, {_Source, ReceivedPacket}} when ReceivedPacket =:= Packet ->
            ok;
        {select, {select_info, recvfrom, Ref}} ->
            receive
                {'$socket', Socket, select, Ref} ->
                    receive_loop_nowait_ref(Socket, Packet)
            after 5000 ->
                {error, timeout}
            end;
        {error, _} = Error ->
            io:format("Error on recv: ~p~n", [Error]),
            Error
    end.

test_setopt_getopt() ->
    {ok, Socket} = socket:open(inet, dgram, udp),
    {ok, dgram} = socket:getopt(Socket, {socket, type}),
    ok = socket:setopt(Socket, {socket, reuseaddr}, true),
    ok = socket:close(Socket),
    {error, closed} = socket:getopt(Socket, {socket, type}),
    {error, closed} = socket:setopt(Socket, {socket, reuseaddr}, true),
    ok.

test_multicast() ->
    {ok, SocketRecv} = socket:open(inet, dgram, udp),
    SocketRecvAddr = #{
        family => inet, addr => {0, 0, 0, 0}, port => 8042
    },
    ok = socket:setopt(SocketRecv, {socket, reuseaddr}, true),
    ok = socket:bind(SocketRecv, SocketRecvAddr),
    ok = socket:setopt(SocketRecv, {ip, add_membership}, #{
        multiaddr => {224, 0, 0, 42}, interface => {0, 0, 0, 0}
    }),

    {ok, SocketSender} = socket:open(inet, dgram, udp),
    ok = socket:sendto(SocketSender, <<"42">>, #{
        family => inet, addr => {224, 0, 0, 42}, port => 8042
    }),
    {ok, SocketSenderAddr} = socket:sockname(SocketSender),
    SocketSenderAddrPort = maps:get(port, SocketSenderAddr),

    {ok, {SocketSenderAddrFrom, <<"42">>}} = socket:recvfrom(SocketRecv, 2, 500),
    {error, timeout} = socket:recvfrom(SocketRecv, 2, 0),
    SocketSenderAddrPort = maps:get(port, SocketSenderAddrFrom),

    ok = socket:sendto(SocketRecv, <<"43">>, #{
        family => inet, addr => {224, 0, 0, 42}, port => 8042
    }),
    {ok, {SocketRecvAddrFrom, <<"43">>}} = socket:recvfrom(SocketRecv, 2, 500),
    {error, timeout} = socket:recvfrom(SocketRecv, 2, 0),
    8042 = maps:get(port, SocketRecvAddrFrom),

    ok = socket:close(SocketRecv),
    ok = socket:close(SocketSender),
    ok.

execute_command("BEAM", Command) ->
    os:cmd(Command);
execute_command("ATOM", Command) ->
    {ok, _, Fd} = atomvm:subprocess("/bin/sh", ["sh", "-c", Command], undefined, [stdout]),
    Result = loop_read(Fd, []),
    ok = atomvm:posix_close(Fd),
    Result.

loop_read(Fd, Acc) ->
    case atomvm:posix_read(Fd, 10) of
        eof ->
            lists:flatten(lists:reverse(Acc));
        {error, eintr} ->
            % used with lldb ;-)
            loop_read(Fd, Acc);
        {ok, Line} ->
            loop_read(Fd, [binary_to_list(Line) | Acc])
    end.
