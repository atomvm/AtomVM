%% This file is part of AtomVM.
%%
%% Copyright (c) 2023 <winford@object.stream>
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%

-module(wifi_scan).

-export([start/0]).

start() ->
    scan_passive([show_hidden, {dwell, 1000}]),
    scan_active([{dwell, 500}]).

scan_active(Config) ->
    io:format(
        "~nStarting active scan with configuration ~p, this may take some time depending on dwell ms used.~n~n",
        [Config]
    ),
    BeginTime = erlang:monotonic_time(millisecond),
    {ok, {Num, Networks}} = network:wifi_scan(Config),
    io:format("Active scan found ~p networks in ~pms.~n", [
        Num, erlang:monotonic_time(millisecond) - BeginTime
    ]),
    lists:foreach(
        fun(_Network = {SSID, [{rssi, DBm}, {authmode, Mode}, {bssid, BSSID}, {channel, Number}]}) ->
            io:format(
                "Network: ~p, BSSID: ~p, signal ~p dBm, Security: ~p, channel ~p~n",
                [SSID, binary:encode_hex(BSSID), DBm, Mode, Number]
            )
        end,
        Networks
    ).

scan_passive(Config) ->
    io:format(
        "~nStarting passive scan with configuration: ~p, this may take some time depending on dwell ms used.~n~n",
        [Config]
    ),
    Opts = lists:flatten([passive | Config]),
    BeginTime = erlang:monotonic_time(millisecond),
    ScanResults = network:wifi_scan(Opts),
    {ok, {Num, Networks}} = ScanResults,
    io:format("Passive scan found ~p networks in ~pms.~n", [
        Num, erlang:monotonic_time(millisecond) - BeginTime
    ]),
    lists:foreach(
        fun(_Network = {SSID, [{rssi, DBm}, {authmode, Mode}, {bssid, BSSID}, {channel, Number}]}) ->
            io:format(
                "Network: ~p, BSSID: ~p, signal ~p dBm, Security: ~p, channel ~p~n",
                [SSID, binary:encode_hex(BSSID), DBm, Mode, Number]
            )
        end,
        Networks
    ).
