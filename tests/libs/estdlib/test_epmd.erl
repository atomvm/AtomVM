%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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

-module(test_epmd).

-export([test/0]).

-define(EPMD_PORT, 4369).

test() ->
    case stop_epmd() of
        ok ->
            {ok, Pid} = epmd:start_link([]),
            ok = test_failed_alive2_response(Pid),
            ok = test_client(),
            ok = test_two_clients(),
            MonitorRef = monitor(process, Pid),
            unlink(Pid),
            exit(Pid, shutdown),
            ok =
                receive
                    {'DOWN', MonitorRef, process, Pid, shutdown} -> ok
                after 5000 -> timeout
                end,
            ok;
        {error, not_found} ->
            ok
    end,
    case start_epmd() of
        ok ->
            ok = test_client(),
            ok = test_two_clients();
        {error, not_found} ->
            ok
    end,
    ok.

test_failed_alive2_response(Pid) ->
    case erlang:system_info(machine) of
        "BEAM" ->
            test_failed_alive2_response_beam(Pid);
        "ATOM" ->
            % Deterministic fault injection needs OTP's public socket registry
            % and socket monitors, which AtomVM does not currently implement.
            ok
    end.

test_failed_alive2_response_beam(Pid) ->
    {ok, Existing} = connect_epmd(),
    ok = send_alive2(Existing, <<"test_epmd_existing">>, 12345),
    {ok, <<118, 0, Creation:32>>} = socket:recv(Existing, 6, 5000),
    {ok, Failed} = connect_epmd(),
    {ok, Peer} = socket:sockname(Failed),
    Deadline = erlang:monotonic_time(millisecond) + 5000,
    ServerSocket = registration_socket(Pid, Peer, Deadline),
    ClosedRef = socket:monitor(ServerSocket),
    % Disable only the response direction, before submitting the request. The
    % server can still read ALIVE2 and arm its recv select, but sending the
    % response must fail. This avoids racing a peer disconnect against send.
    ok = socket:shutdown(ServerSocket, write),
    ok = send_alive2(Failed, <<"test_epmd_failed">>, 12346),
    receive
        {'DOWN', ClosedRef, socket, ServerSocket, _Reason} -> ok
    after 5000 -> error(registration_socket_not_closed)
    end,
    ok = socket:close(Failed),
    % Exercise the public protocol rather than inspecting epmd's private state.
    {port, 12345, 6} = erl_epmd:port_please("test_epmd_existing", "localhost"),
    noport = erl_epmd:port_please("test_epmd_failed", "localhost"),
    {ok, Names} = erl_epmd:names("localhost"),
    true = lists:member({"test_epmd_existing", 12345}, Names),
    false = lists:member({"test_epmd_failed", 12346}, Names),
    {ok, Retry} = connect_epmd(),
    ok = send_alive2(Retry, <<"test_epmd_failed">>, 12346),
    {ok, <<118, 0, NextCreation:32>>} = socket:recv(Retry, 6, 5000),
    NextCreation = (Creation + 1) rem 16#ffffffff,
    {port, 12346, 6} = erl_epmd:port_please("test_epmd_failed", "localhost"),
    ok = socket:close(Retry),
    ok = socket:close(Existing),
    ok.

connect_epmd() ->
    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = socket:connect(Socket, #{family => inet, addr => loopback, port => ?EPMD_PORT}),
    {ok, Socket}.

send_alive2(Socket, Name, Port) ->
    NameLen = byte_size(Name),
    Request = <<120, Port:16, 77, 0, 6:16, 6:16, NameLen:16, Name/binary, 0:16>>,
    socket:send(Socket, <<(byte_size(Request)):16, Request/binary>>).

registration_socket(Pid, Peer, Deadline) ->
    case [S || S <- socket:which_sockets(Pid), socket:peername(S) =:= {ok, Peer}] of
        [Socket] ->
            Socket;
        [] ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true ->
                    erlang:yield(),
                    registration_socket(Pid, Peer, Deadline);
                false ->
                    error(registration_socket_not_accepted)
            end
    end.

has_epmd(Platform) ->
    has_command(Platform, "epmd").

has_command("BEAM", Command) ->
    R = os:cmd("command -v " ++ Command),
    R =/= [];
has_command("ATOM", Command) ->
    {ok, _, Fd} = atomvm:subprocess("/bin/sh", ["sh", "-c", "command -v " ++ Command], undefined, [
        stdout
    ]),
    Result =
        case atomvm:posix_read(Fd, 200) of
            eof -> false;
            {ok, _Line} -> true
        end,
    ok = atomvm:posix_close(Fd),
    Result.

ensure_epmd("BEAM") ->
    Result = os:cmd("epmd -daemon"),
    io:format("~s\n", [Result]),
    ok;
ensure_epmd("ATOM") ->
    {ok, _, Fd} = atomvm:subprocess("/bin/sh", ["sh", "-c", "epmd -daemon 2>&1"], undefined, [
        stdout
    ]),
    print_loop(Fd),
    ok = atomvm:posix_close(Fd),
    ok.

ensure_epmd_connect_loop(0) ->
    io:format("Coult not connect to epmd\n"),
    {error, timeout};
ensure_epmd_connect_loop(N) ->
    case erl_epmd:names("127.0.0.1") of
        {ok, _} ->
            ok;
        {error, Reason} ->
            io:format("Could not connect to epmd -- error = ~p, retrying (N = ~p)\n", [Reason, N]),
            timer:sleep(500),
            ensure_epmd_connect_loop(N - 1)
    end.

print_loop(Fd) ->
    case atomvm:posix_read(Fd, 200) of
        eof ->
            ok;
        {ok, Line} ->
            io:format("~s", [Line]),
            print_loop(Fd)
    end.

stop_epmd("BEAM") ->
    case os:cmd("epmd -kill") of
        "Killed\n" ->
            timer:sleep(500),
            ok;
        "epmd: Cannot connect to local epmd\n" ->
            ok;
        "Killing not allowed - " ->
            {error, not_allowed}
    end;
stop_epmd("ATOM") ->
    {ok, _, Fd} = atomvm:subprocess("/bin/sh", ["sh", "-c", "epmd -kill 2>&1"], undefined, [stdout]),
    Result =
        case atomvm:posix_read(Fd, 200) of
            eof ->
                {error, eof};
            {ok, <<"Killed\n">>} ->
                timer:sleep(500),
                ok;
            {ok, <<"epmd: Cannot connect to local epmd\n">>} ->
                ok;
            {ok, <<"Killing not allowed - ", _/binary>>} ->
                {error, not_allowed}
        end,
    ok = atomvm:posix_close(Fd),
    Result.

start_epmd() ->
    Platform = erlang:system_info(machine),
    case has_epmd(Platform) of
        true ->
            start_epmd(Platform, 5);
        false ->
            {error, not_found}
    end.

start_epmd(_Platform, 0) ->
    exit(timeout);
start_epmd(Platform, N) ->
    ok = ensure_epmd(Platform),
    % on CI, epmd may be slow to accept connections
    case ensure_epmd_connect_loop(5) of
        ok ->
            ok;
        {error, timeout} ->
            stop_epmd(Platform),
            start_epmd(Platform, N - 1)
    end.

stop_epmd() ->
    Platform = erlang:system_info(machine),
    case has_epmd(Platform) of
        true ->
            stop_epmd(Platform);
        false ->
            {error, not_found}
    end.

test_client() ->
    {ok, Pid1} = erl_epmd:start_link(),
    noport = erl_epmd:port_please("test_epmd", "host.invalid"),
    noport = erl_epmd:port_please("test_epmd", "localhost"),
    {ok, Creation1} = erl_epmd:register_node("test_epmd", 12345),
    {port, 12345, 6} = erl_epmd:port_please("test_epmd", "localhost"),
    {error, already_registered} = erl_epmd:register_node("test_epmd", 12345),
    {error, already_registered} = erl_epmd:register_node("test_epmd_new", 12346),
    {ok, Names} = erl_epmd:names("localhost"),
    true = lists:member({"test_epmd", 12345}, Names),
    MonitorRef1 = monitor(process, Pid1),
    unlink(Pid1),
    erl_epmd:stop(),
    shutdown =
        receive
            {'DOWN', MonitorRef1, process, Pid1, Reason1} -> Reason1
        after 5000 -> timeout
        end,

    {ok, Pid2} = erl_epmd:start_link(),
    noport = erl_epmd:port_please("test_epmd", "localhost"),
    {ok, Creation2} = erl_epmd:register_node("test_epmd", 12345),
    true = Creation1 =/= Creation2,
    MonitorRef2 = monitor(process, Pid2),
    unlink(Pid2),
    erl_epmd:stop(),
    shutdown =
        receive
            {'DOWN', MonitorRef2, process, Pid2, Reason2} -> Reason2
        after 5000 -> timeout
        end,

    ok.

test_two_clients() ->
    {ok, Pid1} = erl_epmd:start_link(),
    {ok, _Creation1} = erl_epmd:register_node("test_epmd_1", 12345),
    unregister(erl_epmd),
    {ok, Pid2} = erl_epmd:start_link(),
    {ok, _Creation2} = erl_epmd:register_node("test_epmd_2", 12346),
    {ok, Names} = erl_epmd:names("localhost"),
    true = lists:member({"test_epmd_1", 12345}, Names),
    true = lists:member({"test_epmd_2", 12346}, Names),
    unlink(Pid2),
    MonitorRef2 = monitor(process, Pid2),
    erl_epmd:stop(),
    shutdown =
        receive
            {'DOWN', MonitorRef2, process, Pid2, Reason2} -> Reason2
        after 5000 -> timeout
        end,
    register(erl_epmd, Pid1),
    MonitorRef1 = monitor(process, Pid1),
    unlink(Pid1),
    erl_epmd:stop(),
    shutdown =
        receive
            {'DOWN', MonitorRef1, process, Pid1, Reason1} -> Reason1
        after 5000 -> timeout
        end,
    ok.
