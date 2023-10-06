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

-module(picow_wifi_sta).

-export([start/0]).

start() ->
    Config = [
        {sta, [
            {ssid, <<"myssid">>},
            {psk, <<"mypsk">>},
            {connected, fun connected/0},
            {got_ip, fun got_ip/1},
            {disconnected, fun disconnected/0}
        ]}
    ],
    case network:start(Config) of
        {ok, _Pid} ->
            timer:sleep(infinity);
        Error ->
            erlang:display(Error)
    end.

connected() ->
    io:format("Connected to access point.\n").

got_ip({IPv4, Netmask, Gateway}) ->
    io:format("Got IP: ip=~p, netmask=~p, gateway=~p.\n", [IPv4, Netmask, Gateway]).

disconnected() ->
    io:format("Disconnected from access point.\n").
