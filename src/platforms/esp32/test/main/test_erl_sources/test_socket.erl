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
    Socket = open_port({spawn, "socket"}, []),
    Params = [
        {proto, tcp},
        {connect, true},
        {controlling_process, self()},
        {address, "github.com"},
        {port, 80},
        {active, true},
        {buffer, 512},
        {timeout, 30000},
        binary
    ],
    ok = call(Socket, {init, Params}, 30000),
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
        receive
            {tcp, Socket, <<"HTTP/1.1 301", _/binary>>} -> ok;
            {tcp, Socket, Packet} -> {unexpected_packet, Packet};
            UnexpectedAfterSend -> {unexpected_message_after_send, UnexpectedAfterSend}
        after 10000 ->
            {receive_packet, timeout}
        end,
    ok =
        receive
            {tcp_closed, Socket} ->
                ok;
            UnexpectedAfterPacket ->
                {unexpected_message_after_packet, Socket, UnexpectedAfterPacket}
        after 10000 ->
            {closed_socket, timeout}
        end,
    % In current implementation, socket is closed and process is terminated
    % so we cannot close it.
    0.

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
