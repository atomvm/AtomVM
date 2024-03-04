%
% This file is part of AtomVM.
%
% Copyright 2020 Fred Dushin <fred@dushin.net>
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

-module(test_wifi_example).

-export([start/0]).

start() ->
    case verify_platform(atomvm:platform()) of
        ok ->
            start_network();
        Error ->
            Error
    end.

start_network() ->
    Config = [
        {ap, [
            {ap_started, fun ap_started/0},
            {sta_connected, fun sta_connected/1},
            {sta_ip_assigned, fun sta_ip_assigned/1},
            {sta_disconnected, fun sta_disconnected/1}
            | []
        ]},
        {sta, [
            {connected, fun connected/0},
            {got_ip, fun got_ip/1},
            {disconnected, fun disconnected/0}
            | [
                %% {dhcp_hostname, "my_device_name"},
                {ssid, "Wokwi-GUEST"},
                {psk, ""}
            ]
        ]},
        {sntp, [
            {host, "pool.ntp.org"},
            {synchronized, fun sntp_synchronized/1}
        ]}
    ],
    case network:start(Config) of
        {ok, _Pid} ->
            io:format("Network started.~n"),
            timer:sleep(12000),
            ok;
        Error ->
            Error
    end.

ap_started() ->
    io:format("AP started.~n").

sta_connected(Mac) ->
    io:format("STA connected with mac ~p~n", [Mac]).

sta_disconnected(Mac) ->
    io:format("STA disconnected with mac ~p~n", [Mac]).

sta_ip_assigned(Address) ->
    io:format("STA assigned address ~p~n", [Address]).

connected() ->
    io:format("STA connected.~n").

got_ip(IpInfo) ->
    io:format("Got IP: ~p.~n", [IpInfo]),
    spawn(fun loop/0).

disconnected() ->
    io:format("STA disconnected.~n").

sntp_synchronized({TVSec, TVUsec}) ->
    io:format("Synchronized time with SNTP server. TVSec=~p TVUsec=~p~n", [TVSec, TVUsec]).

verify_platform(esp32) ->
    ok;
verify_platform(Platform) ->
    {error, {unsupported_platform, Platform}}.

loop() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    io:format("Date: ~p/~p/~p ~p:~p:~p (~pms)~n", [
        Year, Month, Day, Hour, Minute, Second, erlang:system_time(millisecond)
    ]),
    timer:sleep(200),
    loop().
