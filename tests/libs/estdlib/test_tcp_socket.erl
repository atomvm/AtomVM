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

-module(test_tcp_socket).

-export([test/0]).

test() ->
    ok = test_echo_server(),
    ok = test_shutdown(),
    ok = test_close_by_another_process(),
    ok = test_buf_size(),
    ok = test_timeout(),
    ok = test_recv_nowait(),
    ok = test_accept_nowait(),
    ok = test_setopt_getopt(),
    case get_otp_version() of
        atomvm ->
            ok = test_abandon_select();
        _ ->
            ok
    end,
    ok.

-define(PACKET_SIZE, 7).

test_echo_server() ->
    etest:flush_msg_queue(),

    {ListenSocket, Port} = start_echo_server(0),

    test_send_receive(Port, 10),

    ok = close_listen_socket(ListenSocket).

%%
%% test_shutdown
%%

test_shutdown() ->
    etest:flush_msg_queue(),

    {ListenSocket, Port} = start_echo_server(0),

    ok = test_shutdown_of_client_sockets(Port),

    ok = close_listen_socket(ListenSocket),

    id(ok).

test_shutdown_of_client_sockets(Port) ->
    ok = test_shutdown_of_side(Port, write, <<"echo:01">>),
    ok = test_shutdown_of_side(Port, read_write, <<"echo:02">>),
    ok = test_shutdown_of_side(Port, read, <<"echo:03">>),
    id(ok).

test_shutdown_of_side(Port, Side, Packet) ->
    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = try_connect(Socket, Port, 10),

    ok = socket:shutdown(Socket, Side),
    case Side of
        read ->
            %% read on the socket should fail
            socket:send(Socket, Packet),
            case catch (socket:recv(Socket)) of
                {error, _} ->
                    ok;
                {ok, Data} ->
                    %% On some Linux kernels, shutdown doesn't return an error
                    %% until all buffered data is read.
                    %% C.f. https://stackoverflow.com/questions/740817/behavior-of-shutdownsock-shut-rd-with-tcp
                    %% Second recv will fail
                    case catch (socket:recv(Socket)) of
                        {error, _} ->
                            ok;
                        {ok, Data} ->
                            error({expected_error_on_recv, Side, Data})
                    end
            end;
        _ ->
            %% write on the socket should fail
            case catch (socket:send(Socket, Packet)) of
                {error, _} ->
                    ok;
                {ok, Data1} ->
                    %% Second send will fail
                    case catch (socket:send(Socket, erlang:atom_to_binary(Side, latin1))) of
                        {error, _} ->
                            ok;
                        {ok, Data2} ->
                            error({expected_error_on_send, Side, Data1, Data2})
                    end
            end
    end,

    ok = close_client_socket(Socket),

    id(ok).

test_close_by_another_process() ->
    % socket:recv is blocking and the only way to interrupt it is to close
    % the socket.
    etest:flush_msg_queue(),

    {ListenSocket, Port} = start_echo_server(0),

    {ok, ClientSocket1} = socket:open(inet, stream, tcp),
    ok = try_connect(ClientSocket1, Port, 10),

    spawn_link(fun() ->
        timer:sleep(500),
        ok = socket:close(ClientSocket1)
    end),
    % recv is blocking
    {error, closed} = socket:recv(ClientSocket1, 0, 5000),

    timer:sleep(10),

    ok = close_listen_socket(ListenSocket).

test_buf_size() ->
    etest:flush_msg_queue(),

    {ListenSocket, Port} = start_echo_server(0),

    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = try_connect(Socket, Port, 10),

    %% try a few failures first
    {error, _} = socket:setopt(Socket, {otp, badopt}, any_value),
    {error, _} = socket:setopt(Socket, {otp, rcvbuf}, not_an_int),
    {error, _} = socket:setopt(Socket, {otp, rcvbuf}, -1),

    %% limit the recv buffer size to 5 bytes
    ok = socket:setopt(Socket, {otp, rcvbuf}, 5),
    true = 5 < ?PACKET_SIZE,

    %% we should only be able to receive
    ok = socket:send(Socket, <<"echo:01">>),
    {ok, <<"echo:">>} = socket:recv(Socket, 0, 5000),
    {ok, <<"01">>} = socket:recv(Socket, 0, 5000),
    ok = socket:send(Socket, <<"echo:02">>),
    {ok, <<"echo:">>} = socket:recv(Socket, 0, 5000),
    {ok, <<"02">>} = socket:recv(Socket, 0, 5000),

    %% verify that the socket:recv length parameter takes
    %% precedence over the default
    ok = socket:send(Socket, <<"echo:03">>),
    {ok, <<"echo:03">>} = socket:recv(Socket, ?PACKET_SIZE, 5000),

    ok = close_client_socket(Socket),

    ok = close_listen_socket(ListenSocket).

%%
%% echo_server
%%

start_echo_server(_Port) ->
    {ok, ListenSocket} = socket:open(inet, stream, tcp),

    ok = socket:setopt(ListenSocket, {socket, reuseaddr}, true),
    ok = socket:setopt(ListenSocket, {socket, linger}, #{onoff => true, linger => 0}),

    ok = socket:bind(ListenSocket, #{
        family => inet, addr => loopback, port => 0
    }),

    ok = socket:listen(ListenSocket),

    {ok, #{port := ActualPort}} = socket:sockname(ListenSocket),

    Self = self(),
    spawn(fun() ->
        Self ! ready,
        accept(Self, ListenSocket)
    end),

    receive
        ready ->
            ok
    after 10000 ->
        error({timeout, ?MODULE, ?LINE})
    end,

    {ListenSocket, ActualPort}.

accept(Pid, ListenSocket) ->
    case socket:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> accept(Pid, ListenSocket) end),
            echo(Pid, Socket);
        {error, closed} ->
            Pid ! accept_terminated,
            ok;
        SomethingElse ->
            Pid ! accept_terminated,
            error({unexpected_return_from_accept, SomethingElse})
    end.

echo(Pid, Socket) ->
    case socket:recv(Socket, ?PACKET_SIZE) of
        {ok, <<"echo:", _/binary>> = Packet} ->
            ok = socket:send(Socket, Packet),
            echo(Pid, Socket);
        {ok, <<"wait:", _/binary>> = Packet} ->
            timer:sleep(500),
            ok = socket:send(Socket, Packet),
            echo(Pid, Socket);
        {ok, <<"chnk:", Rest/binary>>} ->
            ok = socket:send(Socket, <<"chnk:">>),
            timer:sleep(500),
            ok = socket:send(Socket, Rest),
            echo(Pid, Socket);
        %% estdlib TODO
        {error, closed} ->
            Pid ! recv_terminated,
            ok;
        %% OTP-24
        {error, econnreset} ->
            Pid ! recv_terminated,
            ok;
        {error, {closed, <<"read">>}} ->
            Pid ! recv_terminated,
            ok;
        SomethingElse ->
            error({unexpected_return_from_recv, SomethingElse})
    end.

close_listen_socket(ListenSocket) ->
    %%
    %% Close the socket, and wait for a signal that we came out of accept
    %%
    ok = socket:close(ListenSocket),
    ok =
        receive
            accept_terminated ->
                ok
        after 1000 ->
            {error, {timeout, accept_terminated}}
        end.

%%
%% send_receive loop
%%

test_send_receive(Port, N) ->
    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = try_connect(Socket, Port, 10),

    ok = send_receive_loop(Socket, N),

    ok = close_client_socket(Socket).

close_client_socket(Socket) ->
    %%
    %% Close the socket, and wait for a signal that we came out of recv
    %%
    ok = socket:close(Socket),
    receive
        recv_terminated ->
            ok
    after 2000 ->
        throw({timeout, waiting, recv_terminated})
    end.

try_connect(_Socket, _Port, 0) ->
    {error, failed_to_connect};
try_connect(Socket, Port, Tries) ->
    case socket:connect(Socket, #{family => inet, addr => loopback, port => Port}) of
        ok ->
            ok;
        {error, _Reason} ->
            timer:sleep(10),
            try_connect(Socket, Port, Tries - 1)
    end.

send_receive_loop(_Socket, 0) ->
    ok;
send_receive_loop(Socket, I) ->
    Packet = list_to_binary(io_lib:format("echo:~2.10.0B", [I])),
    ?PACKET_SIZE = byte_size(Packet),
    case socket:send(Socket, Packet) of
        ok ->
            case socket:recv(Socket) of
                {ok, _OtherPacket} ->
                    send_receive_loop(Socket, I - 1);
                Error ->
                    io:format("Error on recv: ~p~n", [Error]),
                    Error
            end;
        {error, Reason} = Error ->
            io:format("Error on send: ~p~n", [Reason]),
            Error
    end.

receive_loop_nowait(Socket, Packet) when byte_size(Packet) > 0 ->
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
        {select, {{select_info, recv, SelectHandle}, Data}} when is_reference(SelectHandle) ->
            {Data, Rest} = split_binary(Packet, byte_size(Data)),
            receive
                {'$socket', Socket, select, SelectHandle} ->
                    receive_loop_nowait(Socket, Rest)
            after 5000 ->
                {error, timeout}
            end;
        {error, _} = Error ->
            io:format("Error on recv: ~p~n", [Error]),
            Error
    end.

receive_loop_nowait_ref(Socket, Packet) when byte_size(Packet) > 0 ->
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
        {select, {{select_info, recv, Ref}, Data}} ->
            {Data, Rest} = split_binary(Packet, byte_size(Data)),
            receive
                {'$socket', Socket, select, Ref} ->
                    receive_loop_nowait_ref(Socket, Rest)
            after 5000 ->
                {error, timeout}
            end;
        {error, _} = Error ->
            io:format("Error on recv: ~p~n", [Error]),
            Error
    end.

test_timeout() ->
    etest:flush_msg_queue(),

    {ListenSocket, Port} = start_echo_server(0),

    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = try_connect(Socket, Port, 10),

    % receive of two chunks with an infinity timeout
    Packet0 = <<"chnk:00">>,
    ok = socket:send(Socket, Packet0),
    {ok, Packet0} = socket:recv(Socket, ?PACKET_SIZE, infinity),

    % receive of two chunks with a large timeout
    Packet1 = <<"chnk:01">>,
    ok = socket:send(Socket, Packet1),
    {ok, Packet1} = socket:recv(Socket, ?PACKET_SIZE, 5000),

    % receive of two chunks with a small timeout causing a timeout error
    Packet2 = <<"chnk:02">>,
    ok = socket:send(Socket, Packet2),
    {error, Timeout02} = socket:recv(Socket, ?PACKET_SIZE, 250),
    case Timeout02 of
        {timeout, <<"chnk:">>} ->
            % AtomVM usually does return partial data
            {ok, <<"02">>} = socket:recv(Socket, 2, infinity);
        timeout ->
            % BEAM OTP-27 seems to never return partial data
            {ok, <<"chnk:02">>} = socket:recv(Socket, ?PACKET_SIZE, infinity)
    end,

    % receive of two chunks with a null timeout causing a timeout error
    Packet3 = <<"chnk:03">>,
    ok = socket:send(Socket, Packet3),
    timer:sleep(250),
    case socket:recv(Socket, ?PACKET_SIZE, 0) of
        {ok, <<"chnk:">>} ->
            % BEAM OTP-22 to OTP-24 returns this on Linux on the CI.
            {ok, <<"03">>} = socket:recv(Socket, 2);
        {error, Timeout03} ->
            case Timeout03 of
                {timeout, <<"chnk:">>} ->
                    % BEAM OTP-27 seems to always return partial data
                    % AtomVM usually does
                    {ok, <<"03">>} = socket:recv(Socket, 2);
                timeout ->
                    % Depending on scheduling, AtomVM may return no partial data
                    {ok, <<"chnk:03">>} = socket:recv(Socket, ?PACKET_SIZE)
            end
    end,

    % Test recv
    ok = socket:send(Socket, <<"wait:01">>),
    {error, timeout} = socket:recv(Socket, 0, 100),
    {ok, <<"wait:01">>} = socket:recv(Socket, 0, 5000),

    ok = socket:send(Socket, <<"wait:02">>),
    {error, timeout} = socket:recv(Socket, ?PACKET_SIZE, 0),
    {ok, <<"wait:02">>} = socket:recv(Socket, ?PACKET_SIZE, 5000),

    ok = socket:send(Socket, <<"wait:03">>),
    {error, Timeout04} = socket:recv(Socket, 2 * ?PACKET_SIZE, 5000),
    ok =
        case Timeout04 of
            {timeout, <<"wait:03">>} ->
                % AtomVM usually does return partial data
                ok;
            timeout ->
                % BEAM OTP-27 seems to never return partial data
                ok
        end,

    ok = close_client_socket(Socket),
    ok = close_listen_socket(ListenSocket).

test_recv_nowait() ->
    ok = test_recv_nowait(fun receive_loop_nowait/2),
    ok = test_recv_nowait(fun receive_loop_nowait_ref/2),
    ok.

test_recv_nowait(ReceiveFun) ->
    etest:flush_msg_queue(),

    {ListenSocket, Port} = start_echo_server(0),

    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = try_connect(Socket, Port, 10),

    Packet0 = <<"echo:00">>,
    ok = socket:send(Socket, Packet0),
    ok = ReceiveFun(Socket, Packet0),

    Packet1 = <<"wait:00">>,
    ok = socket:send(Socket, Packet1),
    ok = ReceiveFun(Socket, Packet1),

    Packet2 = <<"chnk:00">>,
    ok = socket:send(Socket, Packet2),
    ok = ReceiveFun(Socket, Packet2),

    ok = close_client_socket(Socket),

    ok = close_listen_socket(ListenSocket).

test_accept_nowait() ->
    OTPVersion = get_otp_version(),
    ok = test_accept_nowait(nowait, OTPVersion),
    ok = test_accept_nowait(make_ref(), OTPVersion),
    ok.

% actually since 22.1, but let's simplify here.
test_accept_nowait(_NoWaitRef, Version) when Version =/= atomvm andalso Version < 23 -> ok;
test_accept_nowait(Ref, Version) when
    is_reference(Ref) andalso Version =/= atomvm andalso Version < 24
->
    ok;
test_accept_nowait(NoWaitRef, _Version) ->
    etest:flush_msg_queue(),

    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = socket:setopt(Socket, {socket, reuseaddr}, true),
    ok = socket:setopt(Socket, {socket, linger}, #{onoff => true, linger => 0}),

    ok = socket:bind(Socket, #{
        family => inet, addr => loopback, port => 0
    }),

    ok = socket:listen(Socket),

    {ok, #{port := Port}} = socket:sockname(Socket),

    Parent = self(),
    {Child, MonitorRef} = spawn_opt(
        fun() ->
            {select, {select_info, accept, Ref}} = socket:accept(Socket, NoWaitRef),
            Parent ! {self(), got_nowait},
            receive
                {'$socket', Socket, select, Ref} ->
                    {ok, ConnSocket} = socket:accept(Socket, 0),
                    socket:send(ConnSocket, <<"hello">>),
                    socket:close(ConnSocket)
            after 5000 ->
                exit(timeout)
            end
        end,
        [link, monitor]
    ),
    ok =
        receive
            {Child, got_nowait} -> ok
        after 5000 -> timeout
        end,
    {ok, ClientSocket} = socket:open(inet, stream, tcp),
    ok = socket:connect(ClientSocket, #{family => inet, addr => loopback, port => Port}),
    {ok, <<"hello">>} = socket:recv(ClientSocket, 5),

    socket:close(ClientSocket),
    ok =
        receive
            {'DOWN', MonitorRef, process, Child, normal} -> ok
        after 5000 ->
            timeout
        end,
    socket:close(Socket),
    ok.

test_setopt_getopt() ->
    {ok, Socket} = socket:open(inet, stream, tcp),
    {ok, stream} = socket:getopt(Socket, {socket, type}),
    ok = socket:setopt(Socket, {socket, reuseaddr}, true),
    ok = socket:close(Socket),
    {error, closed} = socket:getopt(Socket, {socket, type}),
    {error, closed} = socket:setopt(Socket, {socket, reuseaddr}, true),
    ok.

%%
%% abandon_select test
%%

test_abandon_select() ->
    etest:flush_msg_queue(),

    {ok, ListenSocket} = socket:open(inet, stream, tcp),

    ok = socket:setopt(ListenSocket, {socket, reuseaddr}, true),
    ok = socket:setopt(ListenSocket, {socket, linger}, #{onoff => true, linger => 0}),

    ok = socket:bind(ListenSocket, #{
        family => inet, addr => loopback, port => 0
    }),

    ok = socket:listen(ListenSocket),

    Owner = self(),
    spawn(fun() ->
        socket:nif_select_read(ListenSocket, erlang:make_ref()),
        Owner ! done
    end),

    %%
    %% What exactly are we testing here?
    %% That we can abandon a select call and not crash the VM, essentially.
    %% We need to ensure that when the resource is destroyed, the monitor is
    %% dropped.
    %%

    receive
        done ->
            ok
    after 10000 ->
        error({timeout, ?MODULE, ?LINE})
    end,

    erlang:garbage_collect(),
    ok.

get_otp_version() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            list_to_integer(erlang:system_info(otp_release));
        _ ->
            atomvm
    end.

id(X) ->
    X.
