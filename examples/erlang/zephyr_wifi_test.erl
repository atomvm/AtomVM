%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
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

-module(zephyr_wifi_test).

-export([start/0]).

-spec start() -> ok.
start() ->
    %% REPLACE WITH YOUR ACTUAL WI-FI SSID AND PSK (PASSWORD)
    SSID = <<"ssid">>,
    PSK = <<"password">>,

    io:format("Starting network driver... SSID: ~s~n", [SSID]),
    Creds = [
        {ssid, SSID},
        {psk, PSK}
    ],
    case network:wait_for_sta(Creds, 30000) of
        {ok, {Address, Netmask, Gateway}} ->
            io:format("Acquired IP Address: ~s~n", [inet:ntoa(Address)]),
            io:format("Netmask:             ~s~n", [inet:ntoa(Netmask)]),
            io:format("Gateway:             ~s~n", [inet:ntoa(Gateway)]),

            %% Execute the TCP socket test
            socket_test();
        {error, Reason} ->
            io:format("An error occurred starting network: ~p~n", [Reason])
    end,
    ok.

socket_test() ->
    TargetHost = "example.com",
    io:format("Resolving host ~s...~n", [TargetHost]),
    case inet:getaddr(TargetHost, inet) of
        {ok, IPAddress} ->
            io:format("ok, IPAddress~n"),
            io:format("Resolved ~s to ~s~n", [TargetHost, inet:ntoa(IPAddress)]),
            io:format("Opening TCP socket...~n"),
            case socket:open(inet, stream, tcp) of
                {ok, Socket} ->
                    io:format("Connecting to ~s:80...~n", [inet:ntoa(IPAddress)]),
                    case socket:connect(Socket, #{family => inet, addr => IPAddress, port => 80}) of
                        ok ->
                            io:format("Connected! Sending HTTP request...~n"),
                            HTTPRequest = <<"GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n">>,
                            case socket:send(Socket, HTTPRequest) of
                                ok ->
                                    io:format("Request sent. Waiting for response...~n"),
                                    receive_loop(Socket);
                                SendError ->
                                    io:format("Failed to send: ~p~n", [SendError]),
                                    socket:close(Socket)
                            end;
                        ConnectError ->
                            io:format("Failed to connect: ~p~n", [ConnectError]),
                            socket:close(Socket)
                    end;
                {error, OpenError} ->
                    io:format("Failed to open socket: ~p~n", [OpenError])
            end;
        ResolveError ->
            io:format("Failed to resolve ~s: ~p~n", [TargetHost, ResolveError])
    end.

receive_loop(Socket) ->
    case socket:recv(Socket) of
        {ok, Data} ->
            io:format("Received ~p bytes:~n~s~n", [byte_size(Data), Data]),
            receive_loop(Socket);
        {error, closed} ->
            io:format("~nConnection closed by server.~n"),
            socket:close(Socket);
        {error, Reason} ->
            io:format("Error receiving data: ~p~n", [Reason]),
            socket:close(Socket)
    end.
