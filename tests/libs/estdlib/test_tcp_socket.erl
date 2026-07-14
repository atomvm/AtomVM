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
    case erlang:system_info(machine) of
        "ATOM" ->
            ok = test_abandon_select(),
            ok = test_send_backpressure();
        "BEAM" ->
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
    {error, closed} = socket:recv(ClientSocket1, 0, 60000),

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
        %% OTP returns this in some (random) cases
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
    {ok, Packet1} = socket:recv(Socket, ?PACKET_SIZE, 30000),

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
    ok = test_accept_nowait(nowait),
    ok = test_accept_nowait(make_ref()),
    ok.

test_accept_nowait(NoWaitRef) ->
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

%%
%% test_send_backpressure
%%
%% Exercises the write-select mechanism used internally by socket:send/2 to
%% wait for transient send backpressure (a full TCP send buffer) to clear,
%% instead of leaking {error, eagain} or {ok, Rest} to the caller.
%%

test_send_backpressure() ->
    etest:flush_msg_queue(),

    {ok, ListenSocket} = socket:open(inet, stream, tcp),
    ok = socket:setopt(ListenSocket, {socket, reuseaddr}, true),
    ok = socket:setopt(ListenSocket, {socket, linger}, #{onoff => true, linger => 0}),
    ok = socket:bind(ListenSocket, #{family => inet, addr => loopback, port => 0}),
    ok = socket:listen(ListenSocket),
    {ok, #{port := Port}} = socket:sockname(ListenSocket),

    Self = self(),
    Acceptor = spawn_link(fun() -> backpressure_acceptor(Self, ListenSocket) end),

    {ok, ClientSocket} = socket:open(inet, stream, tcp),
    ok = try_connect(ClientSocket, Port, 10),

    ok =
        receive
            {server_socket, _ServerSocket} -> ok
        after 5000 ->
            error({timeout, waiting_for_server_socket})
        end,

    %% Fill the client's send buffer (the server never reads) using the raw
    %% nif_send/2 directly, bypassing socket:send/2's automatic retry, until
    %% we either observe a real {error, eagain} or give up after a generous
    %% number of attempts. Different platforms/kernels size their socket
    %% buffers differently, so we tolerate never observing backpressure
    %% rather than failing the test outright.
    Chunk = binary:copy(<<0>>, 65536),
    {TotalSent, EAgainObserved} = fill_send_buffer(ClientSocket, Chunk, 0, 256),

    ok =
        case EAgainObserved andalso TotalSent > 0 of
            true ->
                %% socket:nif_select_write/2 should let us wait until the
                %% socket becomes writable again. Depending on how the
                %% platform/kernel sizes and accounts for socket buffers, a
                %% small partial drain on the peer may not be enough to
                %% cross the low-water mark for writability, so we have the
                %% acceptor drain everything (as a real reader normally
                %% would) to reliably free up space.
                Ref = erlang:make_ref(),
                ok = socket:nif_select_write(ClientSocket, Ref),

                Acceptor ! {drain_all, self()},
                ok =
                    receive
                        {'$socket', ClientSocket, select, Ref} ->
                            ok
                    after 30000 ->
                        error({timeout, waiting_for_select_write})
                    end,

                %% socket:send/2 should now transparently retry (internally
                %% waiting for write-readiness as needed) and complete
                %% successfully instead of returning {error, eagain} or
                %% {ok, Rest}.
                ok = socket:send(ClientSocket, Chunk),
                ok = socket:close(ClientSocket),
                ok =
                    receive
                        {drained_all, N} when is_integer(N) -> ok
                    after 30000 -> error({timeout, waiting_for_drain_all})
                    end,
                ok;
            false ->
                %% We never managed to fill the send buffer; nothing more to
                %% verify on this platform.
                Acceptor ! {drain_all, self()},
                ok = socket:close(ClientSocket),
                ok =
                    receive
                        {drained_all, N} when is_integer(N) -> ok
                    after 30000 -> error({timeout, waiting_for_drain_all})
                    end,
                ok
        end,

    ok.

%% @private
%% Accepts a single connection and gives control of when to start reading
%% on it to the test process, so the test can deliberately stall the
%% receiver in order to build up send backpressure on the client side.
backpressure_acceptor(Owner, ListenSocket) ->
    {ok, ServerSocket} = socket:accept(ListenSocket),
    Owner ! {server_socket, ServerSocket},
    backpressure_acceptor_loop(Owner, ServerSocket),
    ok = socket:close(ListenSocket).

backpressure_acceptor_loop(Owner, ServerSocket) ->
    receive
        {drain_all, Owner} ->
            Total = recv_until_closed(ServerSocket, 0),
            Owner ! {drained_all, Total},
            ok = socket:close(ServerSocket)
    after 60000 ->
        ok = socket:close(ServerSocket)
    end.

recv_until_closed(Socket, Acc) ->
    case socket:recv(Socket, 0, 30000) of
        {ok, Data} ->
            recv_until_closed(Socket, Acc + byte_size(Data));
        {error, closed} ->
            Acc;
        {error, timeout} ->
            Acc
    end.

%% @private
%% Repeatedly calls the raw nif_send/2 (bypassing socket:send/2's automatic
%% retry) with the same chunk of data, until either an {error, eagain} is
%% observed (returns {TotalSent, true}) or MaxAttempts is reached without
%% ever observing backpressure (returns {TotalSent, false}).
fill_send_buffer(_Socket, _Chunk, TotalSent, 0) ->
    {TotalSent, false};
fill_send_buffer(Socket, Chunk, TotalSent, AttemptsLeft) ->
    case socket:nif_send(Socket, Chunk) of
        ok ->
            fill_send_buffer(Socket, Chunk, TotalSent + byte_size(Chunk), AttemptsLeft - 1);
        {ok, Rest} ->
            Sent = byte_size(Chunk) - byte_size(Rest),
            fill_send_buffer(Socket, Chunk, TotalSent + Sent, AttemptsLeft - 1);
        {error, eagain} ->
            {TotalSent, true};
        {error, Reason} ->
            error({unexpected_send_error, Reason})
    end.

id(X) ->
    X.
