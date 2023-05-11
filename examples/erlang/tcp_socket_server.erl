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

-module(tcp_socket_server).

-export([start/0]).

start() ->
    {ok, ListeningSocket} = socket:open(inet, stream, tcp),
    ok = socket:setopt(ListeningSocket, {socket, reuseaddr}, true),
    ok = socket:setopt(ListeningSocket, {socket, linger}, #{onoff => true, linger => 0}),
    ok = socket:bind(ListeningSocket, #{family => inet, addr => any, port => 44404}),
    ok = socket:listen(ListeningSocket),
    io:format("Listening on ~p.~n", [socket:sockname(ListeningSocket)]),
    spawn(fun() -> accept(ListeningSocket) end),
    timer:sleep(infinity).

accept(ListeningSocket) ->
    io:format("Waiting to accept connection...~n"),
    case socket:accept(ListeningSocket) of
        {ok, ConnectedSocket} ->
            io:format("Accepted connection.  local: ~p peer: ~p~n", [
                socket:sockname(ConnectedSocket), socket:peername(ConnectedSocket)
            ]),
            spawn(fun() -> accept(ListeningSocket) end),
            echo(ConnectedSocket);
        Error ->
            io:format("An error occurred accepting connection: ~p~n", [Error])
    end.

echo(ConnectedSocket) ->
    io:format("Waiting to receive data...~n"),
    case socket:recv(ConnectedSocket) of
        {ok, Data} ->
            erlang:display({refc_binary_info, erlang:system_info(refc_binary_info)}),
            io:format("Received data ~p from ~p.  Echoing back...~n", [
                Data, socket:peername(ConnectedSocket)
            ]),
            {ok, _Rest} = socket:send(ConnectedSocket, Data),
            echo(ConnectedSocket);
        {error, closed} ->
            io:format("Client closed connection.~n");
        Error ->
            io:format("An error occurred waiting on a connected socket: ~p~n", [Error])
    end.
