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
    case erlang:system_info(esp32_chip_info) of
        #{model := esp32_s2} ->
            io:format("test_wifi_scan skipped on this platform."),
            ok;
        #{model := esp32_h2} ->
            io:format("No wifi, test_wifi_scan skipped on this platform."),
            ok;
        _ ->
            wifi_scan_test(),
            cancel_scan_test(),
            network_stop_while_scanning_test()
    end.

wifi_scan_test() ->
    case network:start([{sta, [managed]}]) of
        {ok, _Pid} ->
            try
                case network:wifi_scan([{passive, false}]) of
                    {ok, {Num, Networks}} ->
                        io:format("network:wifi_scan found ~p networks.~n", [Num]),
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
                                    "Network: ~p, BSSID: ~p, signal ~p dBm, Security: ~p, channel ~p~n",
                                    [SSID, BSSID, DBm, Mode, Number]
                                )
                            end,
                            Networks
                        ),
                        true = lists:any(
                            fun(#{ssid := SSID}) -> SSID =:= <<"Wokwi-GUEST">> end, Networks
                        ),
                        ok;
                    {error, Reason} ->
                        erlang:error({scan_failed, Reason})
                end
            after
                ok = network:stop()
            end;
        {error, Reason} ->
            erlang:error({network_start_failed, Reason})
    end.

cancel_scan_test() ->
    case network:start([{sta, [managed]}]) of
        {ok, _Pid} ->
            try
                Self = self(),
                Child = erlang:spawn(fun() ->
                    Self ! {scan_ready, self()},
                    receive
                        go -> ok
                    end,
                    report_scan(Self)
                end),
                receive
                    {scan_ready, Child} ->
                        Child ! go,
                        receive
                            starting_scan ->
                                ok
                        after 5000 ->
                            error({cancel_scan_test, timeout})
                        end
                end,
                ParentResult = network:wifi_scan([{passive, false}]),
                ChildResult =
                    receive
                        {scan_process_result, Child, Result} ->
                            Result
                    after 15000 ->
                        erlang:error(scan_process_timeout)
                    end,
                case {ParentResult, ChildResult} of
                    {{error, busy}, ok} ->
                        ok;
                    {{ok, _}, race_lost} ->
                        ok;
                    {{error, busy}, race_lost} ->
                        erlang:error(both_scans_rejected);
                    {{ok, _}, ok} ->
                        erlang:error(both_scans_succeeded);
                    {{error, Reason}, _} ->
                        erlang:error({parent_scan_failed, Reason});
                    {_, {error, Reason}} ->
                        erlang:error({report_scan_failed, Reason})
                end
            after
                ok = network:stop()
            end;
        {error, Reason} ->
            erlang:error({network_start_failed, Reason})
    end.

report_scan(Owner) ->
    Owner ! starting_scan,
    case network:wifi_scan([{passive, false}]) of
        {ok, {Num, Networks}} when is_integer(Num) andalso is_list(Networks) ->
            Owner ! {scan_process_result, self(), ok};
        {error, busy} ->
            Owner ! {scan_process_result, self(), race_lost};
        {error, Reason} ->
            Owner ! {scan_process_result, self(), {error, Reason}}
    end.

network_stop_while_scanning_test() ->
    erlang:register(stop_test, self()),
    try
        Config = [{sta, [managed, {scan_done, fun scan_callback_handler/1}]}],
        case network:start(Config) of
            {ok, _Pid} ->
                ok = network:wifi_scan([{passive, false}]),
                ok = network:stop(),
                receive
                    {error, canceled} ->
                        ok;
                    {Num, Networks} when is_integer(Num) andalso is_list(Networks) ->
                        ok;
                    Other ->
                        error(Other)
                after 15000 ->
                    erlang:error(scan_callback_timeout)
                end;
            {error, Reason} ->
                erlang:error({network_start_failed, Reason})
        end
    after
        case erlang:whereis(network) of
            undefined ->
                ok;
            _ ->
                ok = network:stop()
        end,
        erlang:unregister(stop_test)
    end,
    ok.

scan_callback_handler(Results) ->
    case erlang:whereis(stop_test) of
        undefined ->
            erlang:error({lost_parent, stop_test});
        Pid ->
            Pid ! Results
    end.
