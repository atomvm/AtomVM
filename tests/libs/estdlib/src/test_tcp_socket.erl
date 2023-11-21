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
    ok = test_override_buf_size(),
    case get_otp_version() of
        atomvm ->
            ok = test_abandon_select();
        _ ->
            ok
    end,
    ok.

test_echo_server() ->
    etest:flush_msg_queue(),

    Port = 44404,
    ListenSocket = start_echo_server(Port),

    test_send_receive(Port, 10),

    close_listen_socket(ListenSocket).

%%
%% test_shutdown
%%

test_shutdown() ->
    etest:flush_msg_queue(),

    Port = 44404,
    ListenSocket = start_echo_server(Port),

    ok = test_shutdown_of_client_sockets(Port),

    ok = close_listen_socket(ListenSocket),

    id(ok).

test_shutdown_of_client_sockets(Port) ->
    ok = test_shutdown_of_side(Port, write),
    ok = test_shutdown_of_side(Port, read_write),
    ok = test_shutdown_of_side(Port, read),
    id(ok).

test_shutdown_of_side(Port, Side) ->
    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = try_connect(Socket, Port, 10),

    ok = socket:shutdown(Socket, Side),
    case Side of
        read ->
            %% read on the socket should fail
            socket:send(Socket, erlang:atom_to_binary(Side, latin1)),
            case catch (socket:recv(Socket)) of
                {error, _} ->
                    ok;
                {ok, Data} ->
                    %% On some Linux kernels, shutdown is not guaranteed to
                    %% result in an error on read.
                    %% C.f. https://stackoverflow.com/questions/740817/behavior-of-shutdownsock-shut-rd-with-tcp
                    erlang:display({warning, expected_error_on_recv, Side, Data}),
                    % error({expected_error_on_recv, Side, Data})
                    ok
            end;
        _ ->
            %% write on the socket should fail
            case catch (socket:send(Socket, erlang:atom_to_binary(Side, latin1))) of
                {error, _} ->
                    ok;
                {ok, Data} ->
                    error({expected_error_on_send, Side, Data})
            end
    end,

    ok = close_client_socket(Socket),

    id(ok).

test_close_by_another_process() ->
    % socket:recv is blocking and the only way to interrupt it is to close
    % the socket.
    etest:flush_msg_queue(),

    Port = 44404,
    ListenSocket = start_echo_server(Port),

    {ok, ClientSocket1} = socket:open(inet, stream, tcp),
    ok = try_connect(ClientSocket1, Port, 10),

    spawn_link(fun() ->
        timer:sleep(500),
        ok = socket:close(ClientSocket1)
    end),
    % recv is blocking
    {error, closed} = socket:recv(ClientSocket1, 0, 5000),

    timer:sleep(10),

    ok = close_listen_socket(ListenSocket),

    id(ok).

check_receive(Socket, Packet, Length, Expect) ->
    case socket:send(Socket, Packet) of
        ok ->
            ok =
                case socket:recv(Socket, Length) of
                    {ok, Expect} ->
                        ok;
                    Error ->
                        io:format("Unexpected value on recv: ~p~n", [Error]),
                        Error
                end;
        {error, Reason} = Error ->
            io:format("Error on send: ~p~n", [Reason]),
            Error
    end.

test_buf_size() ->
    etest:flush_msg_queue(),

    Port = 44404,
    ListenSocket = start_echo_server(Port),

    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = try_connect(Socket, Port, 10),

    %% try a few failures first
    {error, _} = socket:setopt(Socket, {otp, badopt}, any_value),
    {error, _} = socket:setopt(Socket, {otp, rcvbuf}, not_an_int),
    {error, _} = socket:setopt(Socket, {otp, rcvbuf}, -1),

    %% limit the recv buffer size to 10 bytes
    ok = socket:setopt(Socket, {otp, rcvbuf}, 10),

    Packet = "012345678901234567890123456789",

    %% we should only be able to receive
    ok = check_receive(Socket, Packet, 0, <<"0123456789">>),
    ok = check_receive(Socket, Packet, 0, <<"0123456789">>),
    ok = check_receive(Socket, Packet, 0, <<"0123456789">>),

    timer:sleep(10),

    ok = close_client_socket(Socket),

    ok = close_listen_socket(ListenSocket),

    id(ok).

test_override_buf_size() ->
    etest:flush_msg_queue(),

    Port = 44404,
    ListenSocket = start_echo_server(Port),

    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = try_connect(Socket, Port, 10),

    %% limit the recv buffer size to 10 bytes
    ok = socket:setopt(Socket, {otp, rcvbuf}, 10),

    Packet = "012345678901234567890123456789",

    %% verify that the socket:recv length parameter takes
    %% precedence over the default
    ok = check_receive(Socket, Packet, 15, <<"012345678901234">>),
    ok = check_receive(Socket, Packet, 15, <<"567890123456789">>),

    timer:sleep(10),

    ok = close_client_socket(Socket),

    ok = close_listen_socket(ListenSocket),

    id(ok).

%%
%% echo_server
%%

start_echo_server(Port) ->
    {ok, ListenSocket} = socket:open(inet, stream, tcp),

    ok = socket:setopt(ListenSocket, {socket, reuseaddr}, true),
    ok = socket:setopt(ListenSocket, {socket, linger}, #{onoff => true, linger => 0}),

    ok = socket:bind(ListenSocket, #{
        family => inet, addr => loopback, port => Port
    }),

    ok = socket:listen(ListenSocket),

    Self = self(),
    spawn(fun() ->
        Self ! ready,
        accept(Self, ListenSocket)
    end),

    receive
        ready ->
            ok
    end,

    ListenSocket.

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
    case socket:recv(Socket) of
        {ok, Packet} ->
            % Pid ! {packet_received, Packet},
            ok =
                case socket:send(Socket, Packet) of
                    ok ->
                        ok;
                    E ->
                        %% TODO support returning Rest when Packet > buffer_size
                        {unexpected_reply_from_send, E}
                end,
            % Pid ! {packet_echoed, Packet},
            echo(Pid, Socket);
        %% estdlib TODO
        {error, closed} ->
            Pid ! recv_terminated,
            ok;
        %% OTP-24
        {error, econnreset} ->
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
    receive
        accept_terminated ->
            ok
    after 1000 ->
        %%
        %% Closing the listening socket from another process may in some
        %% cases not result in the blocking accept to break out of its
        %% call with an expected return value.  In this case, we will
        %% allow the wait for the `accept_terminated` message to fail
        %% and simply warn the user.  See TODO comment to this effect in
        %% `nif_socket_close` function in `otp_socket.c`
        %%
        erlang:display({warning, timeout, waiting, accept_terminated})
        % throw({timeout, waiting, accept_terminated})
    end,

    ok.

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
    after 1000 ->
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
    Packet = pid_to_list(self()) ++ ":" ++ integer_to_list(I),
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

%%
%% abandon_select test
%%

test_abandon_select() ->
    etest:flush_msg_queue(),

    Port = 44408,
    {ok, ListenSocket} = socket:open(inet, stream, tcp),

    ok = socket:setopt(ListenSocket, {socket, reuseaddr}, true),
    ok = socket:setopt(ListenSocket, {socket, linger}, #{onoff => true, linger => 0}),

    ok = socket:bind(ListenSocket, #{
        family => inet, addr => loopback, port => Port
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
