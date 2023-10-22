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

-module(tcp_socket_client).

-export([start/0]).

-spec start() -> no_return().
start() ->
    {ok, Socket} = socket:open(inet, stream, tcp),
    case socket:connect(Socket, #{family => inet, addr => loopback, port => 44404}) of
        ok ->
            loop(Socket);
        {error, Reason} ->
            io:format("An error occurred connecting to TCP server: ~p~n", [Reason])
    end.

loop(ConnectedSocket) ->
    io:format("Sending data...~n"),
    case socket:send(ConnectedSocket, <<"ping">>) of
        ok ->
            io:format("Sent ping to ~p.~n", [socket:peername(ConnectedSocket)]),
            case socket:recv(ConnectedSocket) of
                {ok, Data} ->
                    io:format("Received ~p.~n", [Data]),
                    timer:sleep(1000),
                    loop(ConnectedSocket);
                Error ->
                    io:format("Error on recv: ~p~n", [Error])
            end;
        {error, closed} ->
            io:format("Client closed connection.~n");
        {error, Reason} ->
            io:format("An error occurred sending data through a connected socket: ~p~n", [Reason])
    end.
