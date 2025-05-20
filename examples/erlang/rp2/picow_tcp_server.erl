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

-module(picow_tcp_server).

-export([start/0]).

-spec start() -> no_return().
start() ->
    Config = [
        {sta, [
            {ssid, <<"myssid">>},
            {psk, <<"mypsk">>},
            {got_ip, fun got_ip/1}
        ]}
    ],
    case network:start(Config) of
        {ok, _Pid} ->
            timer:sleep(infinity);
        Error ->
            erlang:display(Error)
    end.

got_ip({IPv4, Netmask, Gateway}) ->
    io:format("Got IP: ip=~p, netmask=~p, gateway=~p.\n", [IPv4, Netmask, Gateway]),
    setup().

setup() ->
    {ok, ListeningSocket} = socket:open(inet, stream, tcp),

    ok = socket:setopt(ListeningSocket, {socket, reuseaddr}, true),
    ok = socket:setopt(ListeningSocket, {socket, linger}, #{onoff => true, linger => 0}),

    ok = socket:bind(ListeningSocket, #{family => inet, addr => any, port => 44404}),
    ok = socket:listen(ListeningSocket),
    io:format("Listening on ~p.~n", [socket:sockname(ListeningSocket)]),

    spawn(fun() -> accept(ListeningSocket) end).

accept(ListeningSocket) ->
    io:format("Waiting to accept connection...~n"),
    case socket:accept(ListeningSocket) of
        {ok, ConnectedSocket} ->
            io:format("Accepted connection.  local: ~p peer: ~p~n", [
                socket:sockname(ConnectedSocket), socket:peername(ConnectedSocket)
            ]),
            spawn(fun() -> accept(ListeningSocket) end),
            echo(ConnectedSocket);
        {error, Reason} ->
            io:format("An error occurred accepting connection: ~p~n", [Reason])
    end.

-spec echo(ConnectedSocket :: socket:socket()) -> no_return().
echo(ConnectedSocket) ->
    io:format("Waiting to receive data...~n"),
    case socket:recv(ConnectedSocket) of
        {ok, Data} ->
            io:format("Received data ~p from ~p.  Echoing back...~n", [
                Data, socket:peername(ConnectedSocket)
            ]),
            case socket:send(ConnectedSocket, Data) of
                ok ->
                    io:format("All data was sent~n");
                {ok, Rest} ->
                    io:format("Some data was sent.  Remaining: ~p~n", [Rest]);
                {error, Reason} ->
                    io:format("An error occurred sending data: ~p~n", [Reason])
            end,
            echo(ConnectedSocket);
        {error, Reason} ->
            io:format("An error occurred waiting on a connected socket: ~p~n", [Reason])
    end.
