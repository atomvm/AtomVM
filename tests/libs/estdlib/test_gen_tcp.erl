%
% This file is part of AtomVM.
%
% Copyright 2022 Fred Dushin <fred@dushin.net>
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
    ok = test_echo_server(),
    ok = test_accept_parameters(),
    ok.

test_echo_server() ->
    {ok, ListenSocket} = gen_tcp:listen(0, []),
    {ok, {_Address, Port}} = inet:sockname(ListenSocket),

    Self = self(),
    spawn(fun() ->
        Self ! ready,
        accept(Self, ListenSocket)
    end),
    receive
        ready ->
            ok
    end,

    test_send_receive(Port, 100),

    %% TODO bug closing listening socket
    % gen_tcp:close(ListenSocket),

    ok.

accept(Pid, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> accept(Pid, ListenSocket) end),
            echo(Pid, Socket);
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

test_send_receive(Port, N) ->
    {ok, Socket} = gen_tcp:connect(localhost, Port, [{active, true}]),

    loop(Socket, N),

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
    end,
    ok.

sleep(Ms) ->
    receive after Ms -> ok end.


test_accept_parameters() ->
    {ok, ListenSocket} = gen_tcp:listen(0, [{binary, false}, {buffer, 10}]),
    {ok, {_Address, Port}} = inet:sockname(ListenSocket),

    Self = self(),
    spawn(fun() ->
        Self ! ready,
        accept(Self, ListenSocket)
    end),
    receive
        ready ->
            ok
    end,

    {ok, Socket} = gen_tcp:connect(localhost, Port, [{active, true}]),

    sleep(100),

    ok = test_accept_parameters_loop(Socket, 10),

    gen_tcp:close(Socket),
    receive
        server_closed -> ok
    after 1000 -> throw({timeout, waiting, recv, server_closed})
    end,

    ok.

test_accept_parameters_loop(_Socket, 0) ->
    ok;
test_accept_parameters_loop(Socket, I) ->
    Packet = list_to_binary(pid_to_list(self()) ++ ":" ++ integer_to_list(I)),
    ok = gen_tcp:send(Socket, Packet),
    receive
        {tcp_closed, _OtherSocket} ->
            ok;
        {tcp, _OtherSocket, OtherPacket} ->
            case is_binary(OtherPacket) of
                true ->
                    {error, expected_binary_packet};
                false ->
                    loop(Socket, I - 1)
            end
    end,
    ok.
