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

-module(wifi_scan_callback).

-export([start/0]).

start() ->
    Config = [{sta, [managed, {scan_done, fun display_scan_results/1}]}],
    {ok, _Pid} = network:start(Config),
    io:format(
        "~nStarting active scan with configuration ~p, this may take some time depending on dwell ms used.~n~n",
        [Config]
    ),
    case network:wifi_scan() of
        {error, Reason} ->
            io:format("wifi_scan failed for reason ~p", [Reason]);
        ok ->
            timer:sleep(infinity)
    end.

display_scan_results(Results) ->
    case Results of
        {error, Reason} ->
            io:format("wifi_scan failed for reason ~p.~n", [Reason]);
        {Num, Networks} ->
            io:format("wifi_scan callback got ~p results:~n", [Num]),
            lists:foreach(
                fun(
                    _Network = #{
                        ssid := SSID,
                        rssi := DBm,
                        authmode := Mode,
                        bssid := BSSID,
                        channel := Number
                    }
                ) ->
                    io:format(
                        "Network: ~s, BSSID: ~s, signal ~p dBm, Security: ~p, channel ~p~n",
                        [SSID, binary:encode_hex(BSSID), DBm, Mode, Number]
                    )
                end,
                Networks
            )
    end.
