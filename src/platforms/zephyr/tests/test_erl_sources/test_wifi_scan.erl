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
    ok = bad_options_test(),
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
