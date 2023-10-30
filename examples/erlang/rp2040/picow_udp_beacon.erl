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

-module(picow_udp_beacon).

-export([start/0]).

start() ->
    Self = self(),
    Config = [
        {sta, [
            {ssid, <<"myssid">>},
            {psk, <<"mypsk">>},
            {got_ip, fun(IPInfo) -> got_ip(Self, IPInfo) end}
        ]}
    ],
    case network:start(Config) of
        {ok, _Pid} ->
            loop(disconnected);
        Error ->
            erlang:display(Error)
    end.

got_ip(Parent, {IPv4, Netmask, Gateway}) ->
    io:format("Got IP: ip=~p, netmask=~p, gateway=~p.\n", [IPv4, Netmask, Gateway]),
    Parent ! connected.

loop(disconnected) ->
    receive
        connected ->
            {ok, UDPSocket} = socket:open(inet, dgram, udp),
            loop({UDPSocket, 0})
    end;
loop({UDPSocket, N}) ->
    Message = io_lib:format("AtomVM beacon #~B\n", [N]),
    ok = socket:sendto(UDPSocket, Message, #{
        family => inet, port => 4444, addr => {255, 255, 255, 255}
    }),
    io:format("Sent beacon #~B\n", [N]),
    timer:sleep(1000),
    loop({UDPSocket, N + 1}).
