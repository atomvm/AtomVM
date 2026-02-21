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

-module(test_wifi_scan).

-export([start/0]).

start() ->
    ok = network:start([{sta, []}]),
    {ok, {Num, Networks}} = network:wifi_scan(),
    io:format("network:wifi_scan found ~p networks.~n", [Num]),
    lists:foreach(
        fun(_Network = {SSID, [{rssi, DBm}, {authmode, Mode}, {bssid, BSSID}, {channel, Number}]}) ->
            io:format(
                "Network: ~p, BSSID: ~p, signal ~p dBm, Security: ~p, channel ~p~n",
                [SSID, binary:encode_hex(BSSID), DBm, Mode, Number]
            )
        end,
        Networks
    ).
