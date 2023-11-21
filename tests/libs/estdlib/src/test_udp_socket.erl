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
    ok = test_echo_server(),
    ok.

test_echo_server() ->
    Port = 44405,
    {ok, ReceiveSocket} = socket:open(inet, dgram, udp),

    ok = socket:setopt(ReceiveSocket, {socket, reuseaddr}, true),
    ok = socket:setopt(ReceiveSocket, {socket, linger}, #{onoff => true, linger => 0}),

    ok = socket:bind(ReceiveSocket, #{
        family => inet, addr => loopback, port => Port
    }),

    Self = self(),
    spawn(fun() ->
        Self ! ready,
        receive_loop(Self, ReceiveSocket)
    end),

    receive
        ready ->
            ok
    end,

    test_send_receive(Port, 10),

    %%
    %% Close the socket, and wait for a signal that we came out of recvfrom
    %%
    ok = socket:close(ReceiveSocket),
    receive
        recv_terminated -> ok
    after 1000 ->
        %% This is UDP, so raising an error might not be fair here.
        %% Let's just log instead.
        erlang:display({innocuous_udp_timeout, waiting, recv_terminated})
    end,
    ok.

receive_loop(Pid, ReceiveSocket) ->
    case socket:recvfrom(ReceiveSocket) of
        {ok, {_Source, Packet}} ->
            Pid ! {received, Packet},
            receive_loop(Pid, ReceiveSocket);
        {error, closed} ->
            Pid ! recv_terminated;
        SomethingElse ->
            Pid ! recv_terminated,
            error({unexpected_return_from_recv, SomethingElse})
    end.

test_send_receive(Port, N) ->
    {ok, Socket} = socket:open(inet, dgram, udp),

    ok = loop(Socket, Port, N),

    %%
    %% Close the socket
    %%
    ok = socket:close(Socket).

loop(_Socket, _Port, 0) ->
    ok;
loop(Socket, Port, I) ->
    Packet = pid_to_list(self()) ++ ":" ++ integer_to_list(I),
    Dest = #{family => inet, addr => loopback, port => Port},
    case socket:sendto(Socket, Packet, Dest) of
        ok ->
            receive
                {received, _Packet} ->
                    loop(Socket, Port, I - 1)
            end;
        {error, _Reason} = Error ->
            io:format("Error on sendto: ~p~n", [Error]),
            Error
    end.
