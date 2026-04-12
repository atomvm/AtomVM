%% This file is part of AtomVM.
%%
%% Copyright (c) 2024 <winford@object.stream>
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

%% CI sim test network, change to match an expected network name to be discovered by scans
%% when run on hardware.
-define(FIND_NETWORK, <<"Wokwi-GUEST">>).

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
            deny_concurrent_scan_test(),
            network_stop_while_scanning_test(),
            bad_options_test(),
            pid_results_receiver_test()
    end.

wifi_scan_test() ->
    case network:start([{sta, [managed]}]) of
        {ok, _Pid} ->
            try
                case network:wifi_scan([{results, 3}]) of
                    {ok, {Num, Networks}} ->
                        io:format("network:wifi_scan found ~p networks.\n", [Num]),
                        lists:foreach(
                            fun(
                                _Network = #{
                                    authmode := Mode,
                                    bssid := BSSID,
                                    channel := Number,
                                    hidden := Hidden,
                                    rssi := DBm,
                                    ssid := SSID
                                }
                            ) ->
                                io:format(
                                    "Network: ~p, BSSID: ~p, signal ~p dBm, Security: ~p, channel ~p, hidden: ~p\n",
                                    [SSID, BSSID, DBm, Mode, Number, Hidden]
                                )
                            end,
                            Networks
                        ),
                        true = lists:any(
                            fun(#{ssid := SSID}) -> SSID =:= ?FIND_NETWORK end, Networks
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

deny_concurrent_scan_test() ->
    case network:start([{sta, [managed, {scan_dwell_ms, 400}]}]) of
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
                            erlang:error({deny_concurrent_scan_test, timeout})
                        end
                after 5000 ->
                    erlang:error({deny_concurrent_scan_test, scan_ready_timeout})
                end,
                ParentResult = network:wifi_scan([{dwell, 10}, {results, 1}]),
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
                    {{ok, _}, busy_error} ->
                        ok;
                    {{error, busy}, busy_error} ->
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
    case network:wifi_scan([{results, 5}]) of
        {ok, {Num, Networks}} when is_integer(Num) andalso is_list(Networks) ->
            Owner ! {scan_process_result, self(), ok};
        {error, busy} ->
            Owner ! {scan_process_result, self(), busy_error};
        {error, Reason} ->
            Owner ! {scan_process_result, self(), {error, Reason}}
    end.

network_stop_while_scanning_test() ->
    erlang:register(stop_test, self()),
    try
        Config = [{sta, [managed, {scan_done, fun scan_callback_handler/1}]}],
        case network:start(Config) of
            {ok, _Pid} ->
                ok = network:wifi_scan([{dwell, 500}, {results, 1}]),
                case network:stop() of
                    ok ->
                        receive
                            {Num, Networks} when is_integer(Num) andalso is_list(Networks) ->
                                ok;
                            {error, scan_canceled} ->
                                ok;
                            {error, Reason} ->
                                erlang:error({network_stop_while_scanning_test, {failed, Reason}})
                        after 15000 ->
                            erlang:error(scan_callback_timeout)
                        end;
                    Error ->
                        erlang:error({stop_failed, Error})
                end;
            {error, Reason} ->
                erlang:error({network_start_failed, Reason})
        end
    after
        erlang:unregister(stop_test),
        case erlang:whereis(network) of
            undefined ->
                ok;
            _ ->
                ok = network:stop(),
                erlang:error(network_not_stopped)
        end
    end,
    ok.

scan_callback_handler(Results) ->
    case erlang:whereis(stop_test) of
        undefined ->
            erlang:error({lost_parent, stop_test});
        Pid ->
            Pid ! Results
    end.

bad_options_test() ->
    {error, network_not_started} = network:wifi_scan(),
    case network:start([{sta, [managed]}]) of
        {ok, _Pid} ->
            try
                {error, badarg} = network:wifi_scan([{passive, foo}]),
                {error, badarg} = network:wifi_scan([{results, 0}]),
                {error, badarg} = network:wifi_scan([{results, 99}]),
                {error, badarg} = network:wifi_scan([{results, foo}]),
                {error, badarg} = network:wifi_scan([{dwell, foo}]),
                {error, badarg} = network:wifi_scan([{dwell, 0}]),
                {error, badarg} = network:wifi_scan([{dwell, 1501}]),
                ok
            after
                network:stop()
            end;
        {error, Reason} ->
            erlang:error({network_start_failed, Reason})
    end.

pid_results_receiver_test() ->
    Self = self(),
    Ref = make_ref(),
    Receiver = erlang:spawn(fun() -> results_receiver(Ref, Self, true) end),
    receive
        {Ref, receiver_ready} -> ok
    after 5000 ->
        erlang:error({pid_results_receiver_test, {timeout, no_receiver}})
    end,

    try
        Config = [{sta, [managed, {scan_done, Receiver}]}],
        case network:start(Config) of
            {ok, _Pid} ->
                ok = network:wifi_scan(),
                receive
                    {Num, Networks} when is_integer(Num) andalso is_list(Networks) ->
                        ok;
                    {error, Reason} ->
                        erlang:error({pid_results_receiver_test, {failed, Reason}});
                    Error ->
                        erlang:error({pid_results_receiver_test, {unexpected_message, Error}})
                end;
            {error, Reason} ->
                erlang:error({network_start_failed, Reason})
        end
    after
        Receiver ! {Ref, kill},
        receive
            {ok, {stopped, Ref}} -> ok
        after 5000 ->
            erlang:error(scan_receiver_not_stopped)
        end,
        case erlang:whereis(network) of
            undefined ->
                ok;
            _ ->
                ok = network:stop()
        end
    end,
    ok.

results_receiver(Ref, Parent, FirstLoop) ->
    if
        FirstLoop ->
            Parent ! {Ref, receiver_ready};
        true ->
            ok
    end,

    receive
        {Ref, kill} ->
            Parent ! {ok, {stopped, Ref}},
            ok;
        {scan_results, Msg} ->
            Parent ! Msg,
            results_receiver(Ref, Parent, false);
        Unexpected ->
            Parent ! {error, {unexpected_message, Unexpected}},
            results_receiver(Ref, Parent, false)
    end.
