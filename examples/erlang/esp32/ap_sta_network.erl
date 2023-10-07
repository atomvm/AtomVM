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

-module(ap_sta_network).

-export([start/0]).

start() ->
    Config = [
        {ap, [
            %% If an SSID is not specified, AtomVM will default to atomvm-<hexmac>
            %% where <hexmac> is the hexadecimal representation of the factory-supplied
            %% MAC address of the ESP32 device.
            %% {ssid, esp:nvs_get_binary(atomvm, ap_ssid, <<"myssid">>)},
            %% If a password is not specified, the AP network will be open, with
            %% no encryption or authentication (strongly discouraged)
            %% {psk,  esp:nvs_get_binary(atomvm, ap_psk, <<"mypsk">>)},
            {ap_started, fun ap_started/0},
            {sta_connected, fun sta_connected/1},
            {sta_ip_assigned, fun sta_ip_assigned/1},
            {sta_disconnected, fun sta_disconnected/1}
        ]},
        {sta, [
            {ssid, <<"myssid">>},
            {psk, <<"mypsk">>},
            {connected, fun connected/0},
            {got_ip, fun got_ip/1},
            {disconnected, fun disconnected/0}
        ]},
        {sntp, [
            {host, "pool.ntp.org"},
            {synchronized, fun sntp_synchronized/1}
        ]}
    ],
    case network:start(Config) of
        {ok, _Pid} ->
            timer:sleep(infinity);
        Error ->
            erlang:display(Error)
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
    io:format("Got IP: ~p.~n", [IpInfo]).

disconnected() ->
    io:format("STA disconnected.~n").

sntp_synchronized({TVSec, TVUsec}) ->
    io:format("Synchronized time with SNTP server. TVSec=~p TVUsec=~p~n", [TVSec, TVUsec]).
