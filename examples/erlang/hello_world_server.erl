%
% This file is part of AtomVM.
%
% Copyright 2019-2020 Fred Dushin <fred@dushin.net>
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

-module(hello_world_server).

-export([start/0, handle_req/3]).

start() ->
    Self = self(),
    Config = [
        {sta, [
            {ssid, "SSID"},
            {psk, "PSK"},
            {connected, fun() -> Self ! connected end},
            {got_ip, fun(IpInfo) -> Self ! {ok, IpInfo} end},
            {disconnected, fun() -> Self ! disconnected end}
        ]}
    ],
    case network:start(Config) of
        {ok, _Pid} ->
            wait_for_message();
        Error ->
            io:format("An error occurred starting network: ~p~n", [Error])
    end.

handle_req("GET", [], Conn) ->
    Body = <<"<html><body><h1>Hello World</h1></body></html>">>,
    http_server:reply(200, Body, Conn);
handle_req(Method, Path, Conn) ->
    io:format("Method: ~p Path: ~p~n", [Method, Path]),
    Body = <<"<html><body><h1>Not Found</h1></body></html>">>,
    http_server:reply(404, Body, Conn).

wait_for_message() ->
    Router = [
        {"*", ?MODULE, []}
    ],
    receive
        connected ->
            io:format("Connected~n");
        {ok, IpInfo} ->
            io:format("Acquired IP address: ~p~n", [IpInfo]),
            http_server:start_server(8080, Router);
        disconnected ->
            io:format("Disconnected~n")
    after 15000 ->
        ok
    end,
    wait_for_message().
