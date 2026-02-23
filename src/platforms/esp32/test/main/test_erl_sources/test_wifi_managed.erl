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

-module(test_wifi_managed).

-export([start/0]).

start() ->
    case verify_platform(atomvm:platform()) of
        ok ->
            ok = test_managed_start(),
            ok = test_managed_connect(),
            ok = test_sta_status_lifecycle(),
            ok = test_disconnect_and_reconnect(),
            ok = test_connect_new_ap(),
            network:stop(),
            ok;
        Error ->
            Error
    end.

%% Test starting the network driver in managed mode (no immediate connection)
test_managed_start() ->
    Self = self(),
    Config =
        [
            {sta, [
                managed,
                {connected, fun() -> Self ! sta_connected end},
                {got_ip, fun(IpInfo) -> Self ! {got_ip, IpInfo} end},
                {disconnected, fun() -> Self ! sta_disconnected end}
            ]},
            {sntp, [{host, "time.aws.com"}, {synchronized, fun sntp_synchronized/1}]}
        ],
    case network:start(Config) of
        {ok, _Pid} ->
            io:format("Managed network started.~n"),
            %% In managed mode, sta_status should be disconnected (not connecting)
            disconnected = network:sta_status(),
            io:format("test_managed_start OK.~n"),
            ok;
        Error ->
            Error
    end.

%% Test connecting to an AP using sta_connect/1
test_managed_connect() ->
    %% Status should still be disconnected before connect
    disconnected = network:sta_status(),
    ok =
        network:sta_connect([
            {ssid, "Wokwi-GUEST"},
            {psk, ""},
            {sntp, [{host, "pool.ntp.org"}, {synchronized, fun sntp_synchronized/1}]}
        ]),
    %% After initiating connect, status should be connecting
    Status = network:sta_status(),
    case Status of
        connecting ->
            ok;
        associated ->
            ok;
        connected ->
            ok;
        Other ->
            error({unexpected_sta_status, Other})
    end,
    case wait_for_ip(20000) of
        ok -> ok;
        E -> error({waiting_for_ip, E})
    end,
    connected = network:sta_status(),
    io:format("test_managed_connect OK.~n"),
    ok.

%% Test the full sta_status lifecycle
test_sta_status_lifecycle() ->
    %% We should be connected from previous test
    connected = network:sta_status(),
    io:format("test_sta_status_lifecycle OK.~n"),
    ok.

%% Test disconnecting and reconnecting to the same AP
test_disconnect_and_reconnect() ->
    ok = network:sta_disconnect(),
    ok = wait_for_disconnect(5000),
    disconnected = network:sta_status(),
    %% Reconnect using sta_connect/0 (reconnect to last AP)
    ok = network:sta_connect(),
    ok = wait_for_ip(20000),
    connected = network:sta_status(),
    io:format("test_disconnect_and_reconnect OK.~n"),
    ok.

%% Test connecting to a new AP using sta_connect/1 with sta_config
test_connect_new_ap() ->
    ok = network:sta_disconnect(),
    ok = wait_for_disconnect(5000),
    disconnected = network:sta_status(),
    %% Connect again with explicit config
    ok = network:sta_connect([{ssid, "Wokwi-GUEST"}, {psk, ""}]),
    ok = wait_for_ip(20000),
    connected = network:sta_status(),
    io:format("test_connect_new_ap OK.~n"),
    ok.

sntp_synchronized({TVSec, TVUsec}) ->
    io:format("Synchronized time with SNTP server. TVSec=~p TVUsec=~p~n", [TVSec, TVUsec]).

verify_platform(esp32) ->
    ok;
verify_platform(Platform) ->
    {error, {unsupported_platform, Platform}}.

wait_for_ip(Timeout) ->
    receive
        {got_ip, IpInfo} ->
            io:format("Got IP: ~p~n", [IpInfo]),
            ok
    after Timeout ->
        {error, timeout}
    end.

wait_for_disconnect(Timeout) ->
    receive
        sta_disconnected ->
            io:format("STA disconnected.~n"),
            ok
    after Timeout ->
        {error, timeout}
    end.
