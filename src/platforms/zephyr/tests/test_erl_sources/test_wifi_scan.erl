%% This file is part of AtomVM.
%%
%% Copyright 2026 Peter M <petermm@gmail.com>
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%

-module(test_wifi_scan).

-export([start/0]).

-define(FIND_NETWORK, <<"Wokwi-GUEST">>).

start() ->
    ok = wifi_scan_test(),
    ok = deny_concurrent_scan_test(),
    ok = network_stop_while_scanning_test(),
    ok = bad_options_test(),
    ok = pid_results_receiver_test(),
    ok.

wifi_scan_test() ->
    case network:start([{sta, [managed]}]) of
        {ok, _Pid} ->
            try
                case network:wifi_scan([{results, 3}]) of
                    {ok, {Num, Networks}} ->
                        io:format("network:wifi_scan found ~p networks.~n", [Num]),
                        lists:foreach(fun print_network/1, Networks),
                        true = lists:any(fun(#{ssid := SSID}) -> SSID =:= ?FIND_NETWORK end, Networks),
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

print_network(#{
    authmode := Mode,
    bssid := BSSID,
    channel := Number,
    hidden := Hidden,
    rssi := DBm,
    ssid := SSID
}) ->
    io:put_chars([
        "Network: ",
        SSID,
        ", BSSID: ",
        bssid_hex(BSSID),
        ", signal ",
        integer_to_list(DBm),
        " dBm",
        ", Security: ",
        atom_to_list(Mode),
        ", channel ",
        integer_to_list(Number),
        ", hidden: ",
        atom_to_list(Hidden),
        "\n"
    ]).

bssid_hex(<<A, B, C, D, E, F>>) ->
    [
        byte_hex(A),
        $:,
        byte_hex(B),
        $:,
        byte_hex(C),
        $:,
        byte_hex(D),
        $:,
        byte_hex(E),
        $:,
        byte_hex(F)
    ].

byte_hex(B) ->
    [hex_char(B bsr 4), hex_char(B band 16#0F)].

hex_char(N) when N < 10 -> $0 + N;
hex_char(N) -> $a + N - 10.

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
                ok = network:stop()
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
