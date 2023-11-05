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

-module(udp_server_blink).

-export([start/0]).

-define(PIN, 2).
-define(ON, 1).
-define(OFF, 0).

start() ->
    Creds = [
        {ssid, "myssid"},
        {psk, "mypsk"}
    ],
    case network:wait_for_sta(Creds, 30000) of
        {ok, {Address, Netmask, Gateway}} ->
            io:format(
                "Acquired IP address: ~s Netmask: ~s Gateway: ~s~n",
                [to_string(Address), to_string(Netmask), to_string(Gateway)]
            ),
            udp_server_start();
        Error ->
            io:format("An error occurred starting network: ~p~n", [Error])
    end.

udp_server_start() ->
    case gen_udp:open(44404, [{active, true}]) of
        {ok, Socket} ->
            io:format("Opened UDP socket on ~s.~n", [local_address(Socket)]),
            Gpio = gpio:open(),
            gpio:set_direction(Gpio, ?PIN, output),
            loop(Socket, Gpio, 0);
        Error ->
            io:format("An error occurred opening UDP socket: ~p~n", [Error])
    end.

loop(Socket, Gpio, PinState) ->
    io:format("Waiting to receive data...~n"),
    gpio:set_level(Gpio, ?PIN, PinState),
    receive
        {udp, _Socket, Address, Port, Packet} ->
            io:format("Received UDP packet ~p from ~s~n", [Packet, to_string({Address, Port})])
    end,
    loop(Socket, Gpio, 1 - PinState).

local_address(Socket) ->
    {ok, SockName} = inet:sockname(Socket),
    to_string(SockName).

to_string({{A, B, C, D}, Port}) ->
    io_lib:format("~p.~p.~p.~p:~p", [A, B, C, D, Port]);
to_string({A, B, C, D}) ->
    io_lib:format("~p.~p.~p.~p", [A, B, C, D]).
