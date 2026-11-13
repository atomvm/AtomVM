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
    ok = test_send_contracts(),
    case erlang:system_info(machine) of
        "ATOM" ->
            ok = test_independent_read_write_selects(),
            ok = test_close_after_readiness(),
            ok = test_abandon_select();
        "BEAM" ->
            ok
    end,
    ok.

test_close_after_readiness() ->
    lists:foreach(fun test_close_after_readiness/1, [false, true]),
    ok.

test_close_after_readiness(ReadReady) ->
    {ok, ListenSocket} = socket:open(inet, stream, tcp),
    ok = socket:bind(ListenSocket, #{family => inet, addr => loopback, port => 0}),
    ok = socket:listen(ListenSocket),
    {ok, #{port := Port}} = socket:sockname(ListenSocket),
    {ok, ClientSocket} = socket:open(inet, stream, tcp),
    ok = socket:connect(ClientSocket, #{family => inet, addr => loopback, port => Port}),
    {ok, ServerSocket} = socket:accept(ListenSocket),
    case ReadReady of
        true -> ok = socket:send(ClientSocket, <<42>>);
        false -> ok
    end,
    Parent = self(),
    {Owner, Monitor, 0} = start_registered(fun() ->
        ReadRef = make_ref(),
        ok = socket:nif_select_read(ServerSocket, ReadRef),
        case ReadReady of
            true ->
                receive
                    {'$socket', ServerSocket, select, ReadRef} -> ok
                after 5000 -> error(read_readiness_timeout)
                end;
            false ->
                ok
        end,
        WriteRef = make_ref(),
        ok = socket:nif_select_write(ServerSocket, WriteRef),
        receive
            {'$socket', ServerSocket, select, WriteRef} -> ok
        after 5000 -> error(write_readiness_timeout)
        end,
        Parent ! {self(), registered, 0},
        receive
            closed -> ok
        after 5000 -> error(close_timeout)
        end,
        %% A pending reader still needs its abort, but a direction whose
        %% readiness was already consumed must not receive another notification.
        case ReadReady of
            false ->
                receive
                    {'$socket', ServerSocket, abort, {ReadRef, closed}} -> ok
                after 5000 -> error(read_abort_timeout)
                end;
            true ->
                ok
        end,
        receive
            {'$socket', ServerSocket, abort, _} = Message -> error({unexpected_abort, Message})
        after 0 -> ok
        end,
        Parent ! {self(), result, ok}
    end),
    ok = socket:close(ServerSocket),
    Owner ! closed,
    ok = wait_result(Owner),
    normal = wait_down(Owner, Monitor),
    ok = socket:close(ClientSocket),
    ok = socket:close(ListenSocket).

test_independent_read_write_selects() ->
    etest:flush_msg_queue(),
    lists:foreach(
        fun(Order) ->
            ok = test_blocked_read_write(Order, close),
            ok = test_blocked_read_write(Order, kill_writer),
            ok = test_blocked_read_write(Order, drain),
            ok = test_blocked_read_write(Order, read_eof)
        end,
        [reader_first, writer_first]
    ),
    ok.

test_blocked_read_write(Order, Action) ->
    {ok, ListenSocket} = socket:open(inet, stream, tcp),
    ok = socket:setopt(ListenSocket, {socket, reuseaddr}, true),
    ok = socket:bind(ListenSocket, #{family => inet, addr => loopback, port => 0}),
    ok = socket:listen(ListenSocket),
    {ok, #{port := Port}} = socket:sockname(ListenSocket),
    {ok, ClientSocket} = socket:open(inet, stream, tcp),
    ok = socket:connect(ClientSocket, #{family => inet, addr => loopback, port => Port}),
    {ok, ServerSocket} = socket:accept(ListenSocket),

    %% The accepted socket is shared by two selectors and closed by a third
    %% process.  Its peer deliberately does not read until the drain case.
    %% Public nowait sends prove backpressure without inferring it from silence.
    Packet = <<0:65536>>,
    Parent = self(),
    StartReader = fun() ->
        start_registered(fun() ->
            {select, {select_info, recv, _Ref}} = socket:recv(ServerSocket, 1, nowait),
            Parent ! {self(), registered, 0},
            %% Wait through the public receive path so EOF exercises its
            %% selection cleanup while the independent writer is still waiting.
            Result = socket:recv(ServerSocket, 1, 60000),
            Parent ! {self(), result, Result}
        end)
    end,
    StartWriter = fun() ->
        start_registered(fun() ->
            {Buffered, Rest, Ref} = fill_send_buffer(ServerSocket, Packet, 0, 8192),
            Parent ! {self(), registered, Buffered + byte_size(Rest)},
            Result =
                receive
                    {'$socket', ServerSocket, abort, {Ref, closed}} ->
                        {error, closed};
                    closed ->
                        socket:send(ServerSocket, Rest, 0);
                    resume ->
                        receive
                            {'$socket', ServerSocket, select, Ref} -> ok
                        after 60000 -> error(write_readiness_timeout)
                        end,
                        ok = socket:send(ServerSocket, Rest, 60000),
                        socket:send(ServerSocket, <<91, 7, 73, 19>>, 60000)
                after 60000 -> error(writer_command_timeout)
                end,
            Parent ! {self(), result, Result}
        end)
    end,
    {{Reader, ReaderMonitor, 0}, {Writer, WriterMonitor, Buffered}} =
        case Order of
            reader_first ->
                R = StartReader(),
                W = StartWriter(),
                {R, W};
            writer_first ->
                W = StartWriter(),
                R = StartReader(),
                {R, W}
        end,
    Total = Buffered,
    case Action of
        close ->
            ok = socket:close(ServerSocket),
            Writer ! closed,
            {error, closed} = wait_result(Reader),
            {error, closed} = wait_result(Writer),
            normal = wait_down(Writer, WriterMonitor);
        kill_writer ->
            exit(Writer, kill),
            killed = wait_down(Writer, WriterMonitor),
            ok = socket:send(ClientSocket, <<42>>),
            {ok, <<42>>} = wait_result(Reader),
            ok = socket:close(ServerSocket);
        drain ->
            %% Check every byte, including the unsent suffix from a partial send.
            Writer ! resume,
            ok = drain_writer_data(ClientSocket, Total),
            ok = wait_result(Writer),
            normal = wait_down(Writer, WriterMonitor),
            ok = socket:send(ClientSocket, <<42>>),
            {ok, <<42>>} = wait_result(Reader),
            ok = socket:close(ServerSocket);
        read_eof ->
            %% EOF must cancel only read selection, not the blocked writer.
            ok = socket:shutdown(ClientSocket, write),
            {error, closed} = wait_result(Reader),
            Writer ! resume,
            ok = drain_writer_data(ClientSocket, Total),
            ok = wait_result(Writer),
            normal = wait_down(Writer, WriterMonitor),
            ok = socket:close(ServerSocket)
    end,
    normal = wait_down(Reader, ReaderMonitor),
    ok = socket:close(ClientSocket),
    ok = socket:close(ListenSocket).

%% Bound the amount of data queued even on hosts with large TCP buffers.
fill_send_buffer(_Socket, _Packet, _Buffered, 0) ->
    error(send_buffer_did_not_block);
fill_send_buffer(Socket, Packet, Buffered, Attempts) ->
    Ref = make_ref(),
    case socket:send(Socket, Packet, Ref) of
        ok ->
            fill_send_buffer(Socket, <<0:65536>>, Buffered + byte_size(Packet), Attempts - 1);
        {select, {{select_info, send, Ref}, Rest}} ->
            {Buffered + byte_size(Packet) - byte_size(Rest), Rest, Ref};
        {select, {select_info, send, Ref}} ->
            {Buffered, Packet, Ref}
    end.

start_registered(Fun) ->
    {Pid, Monitor} = spawn_opt(Fun, [monitor]),
    Value = wait_registered(Pid),
    {Pid, Monitor, Value}.

wait_result(Pid) ->
    receive
        {Pid, result, Result} -> Result
    after 60000 ->
        error({timeout, result, Pid})
    end.

drain_writer_data(Socket, Total) ->
    ok = drain_zero_bytes(Socket, Total),
    {ok, <<91, 7, 73, 19>>} = socket:recv(Socket, 4, 60000),
    ok.

drain_zero_bytes(_Socket, 0) ->
    ok;
drain_zero_bytes(Socket, Remaining) ->
    Size = erlang:min(Remaining, 8192),
    {ok, Data} = socket:recv(Socket, Size, 60000),
    Data = <<0:(Size * 8)>>,
    drain_zero_bytes(Socket, Remaining - Size).

wait_registered(Pid) ->
    receive
        {Pid, registered, Value} -> Value
    after 5000 ->
        error({timeout, registered, Pid})
    end.

wait_down(Pid, Monitor) ->
    receive
        {'DOWN', Monitor, process, Pid, Reason} -> Reason
    after 5000 ->
        error({timeout, down, Pid})
    end.

%% These contracts use only public socket calls and also run on BEAM. A large
%% patterned payload makes partial progress observable; every accepted byte and
%% the returned suffix are checked, rather than guessing how big TCP buffers are.
test_send_contracts() ->
    lists:foreach(fun test_send_timeout/1, [{0, same}, {30, same}, {30, separate}]),
    lists:foreach(fun test_send_nowait/1, [nowait, make_ref()]),
    ok = test_send_closed_after_partial(),
    {Listen, Sender, Peer} = send_pair(),
    ok = socket:close(Sender),
    lists:foreach(
        fun(Timeout) -> {error, closed} = socket:send(Sender, <<42>>, Timeout) end,
        [0, 30, nowait, make_ref()]
    ),
    ok = socket:close(Peer),
    ok = socket:close(Listen).

test_send_closed_after_partial() ->
    {Listen, Sender, Peer} = send_pair(),
    Data = send_payload(),
    Parent = self(),
    {Writer, Monitor} = spawn_opt(
        fun() ->
            Parent ! {self(), result, socket:send(Sender, Data)}
        end,
        [link, monitor]
    ),
    %% Reading an actual prefix proves progress before closing the local socket;
    %% no sleep is needed to guess whether the writer has started its send.
    {Prefix, _} = split_binary(Data, 65536),
    {ok, Prefix} = socket:recv(Peer, 65536, 60000),
    ok = socket:close(Sender),
    {error, {closed, Rest}} = wait_result(Writer),
    true = byte_size(Rest) > 0,
    true = byte_size(Rest) =< byte_size(Data) - byte_size(Prefix),
    {_, Rest} = split_binary(Data, byte_size(Data) - byte_size(Rest)),
    normal = wait_down(Writer, Monitor),
    ok = socket:close(Peer),
    ok = socket:close(Listen).

send_pair() ->
    {ok, Listen} = socket:open(inet, stream, tcp),
    ok = socket:bind(Listen, #{family => inet, addr => loopback, port => 0}),
    ok = socket:listen(Listen),
    {ok, #{port := Port}} = socket:sockname(Listen),
    {ok, Sender} = socket:open(inet, stream, tcp),
    ok = socket:connect(Sender, #{family => inet, addr => loopback, port => Port}),
    {ok, Peer} = socket:accept(Listen),
    {Listen, Sender, Peer}.

send_payload() ->
    binary:copy(<<91, 7, 73, 19, 0, 255, 42>>, 2400000).

test_send_timeout({Timeout, ReadOwner}) ->
    {Listen, Sender, Peer} = send_pair(),
    Parent = self(),
    ReadRef = make_ref(),
    Reader =
        case ReadOwner of
            same ->
                {select, {select_info, recv, ReadRef}} = socket:recv(Sender, 1, ReadRef),
                self();
            separate ->
                {Pid, Mon, 0} = start_registered(fun() ->
                    {select, {select_info, recv, ReadRef}} = socket:recv(Sender, 1, ReadRef),
                    Parent ! {self(), registered, 0},
                    ok = receive_read_ready(Sender, ReadRef),
                    Parent ! {self(), result, ok}
                end),
                {Pid, Mon}
        end,
    Data = send_payload(),
    Started = erlang:monotonic_time(millisecond),
    {error, {timeout, Rest}} = socket:send(Sender, Data, Timeout),
    %% A generous bound catches an infinite/restarted timeout without making
    %% scheduler latency part of the API contract.
    true = erlang:monotonic_time(millisecond) - Started < 60000,
    true = byte_size(Rest) > 0,
    true = byte_size(Rest) < byte_size(Data),
    Sent = byte_size(Data) - byte_size(Rest),
    {_, Rest} = split_binary(Data, Sent),
    %% ACKs can free more space after the first timeout. Continue accounting for
    %% that progress until the public API explicitly reports NO bytes written.
    ResumeRest = send_until_timeout(Sender, Rest, Timeout, 256),
    %% Write timeout cancellation must preserve the ORIGINAL read reference,
    %% even when both directions belong to the very same process.
    ok = socket:send(Peer, <<42>>, 60000),
    case Reader of
        {ReaderPid, ReaderMon} ->
            ok = wait_result(ReaderPid),
            normal = wait_down(ReaderPid, ReaderMon);
        _ ->
            ok = receive_read_ready(Sender, ReadRef)
    end,
    {Drainer, Monitor} = spawn_opt(
        fun() ->
            ok = receive_exact_data(Peer, Data),
            Parent ! {self(), result, ok}
        end,
        [link, monitor]
    ),
    ok = socket:send(Sender, ResumeRest, 60000),
    ok = wait_result(Drainer),
    normal = wait_down(Drainer, Monitor),
    ok = socket:close(Sender),
    ok = socket:close(Peer),
    ok = socket:close(Listen).

send_until_timeout(_Socket, _Data, _Timeout, 0) ->
    error(send_buffer_did_not_fill);
send_until_timeout(Socket, Data, Timeout, Attempts) ->
    case socket:send(Socket, Data, Timeout) of
        {error, timeout} ->
            Data;
        {error, {timeout, Rest}} ->
            true = byte_size(Rest) > 0,
            true = byte_size(Rest) < byte_size(Data),
            {_, Rest} = split_binary(Data, byte_size(Data) - byte_size(Rest)),
            send_until_timeout(Socket, Rest, Timeout, Attempts - 1)
    end.

receive_read_ready(Socket, Ref) ->
    receive
        {'$socket', Socket, select, Ref} -> ok
    after 60000 -> error(read_selection_lost_after_write_timeout)
    end,
    {ok, <<42>>} = socket:recv(Socket, 1, 60000),
    ok.

test_send_nowait(Timeout) ->
    {Listen, Sender, Peer} = send_pair(),
    Data = send_payload(),
    {select, {{select_info, _SendTag, Ref}, Rest}} = socket:send(Sender, Data, Timeout),
    true = is_reference(Ref),
    case Timeout of
        nowait -> ok;
        Ref -> ok
    end,
    true = byte_size(Rest) > 0,
    true = byte_size(Rest) < byte_size(Data),
    {_, Rest} = split_binary(Data, byte_size(Data) - byte_size(Rest)),
    Parent = self(),
    {Drainer, Monitor} = spawn_opt(
        fun() ->
            ok = receive_exact_data(Peer, Data),
            Parent ! {self(), result, ok}
        end,
        [link, monitor]
    ),
    receive
        {'$socket', Sender, select, Ref} -> ok
    after 60000 -> error(send_readiness_timeout)
    end,
    ok = socket:send(Sender, Rest, 60000),
    ok = wait_result(Drainer),
    normal = wait_down(Drainer, Monitor),
    ok = socket:close(Sender),
    ok = socket:close(Peer),
    ok = socket:close(Listen).

receive_exact_data(_Socket, <<>>) ->
    ok;
receive_exact_data(Socket, Expected) ->
    Size = erlang:min(byte_size(Expected), 65536),
    {Chunk, Rest} = split_binary(Expected, Size),
    {ok, Chunk} = socket:recv(Socket, Size, 60000),
    receive_exact_data(Socket, Rest).

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

id(X) ->
    X.
