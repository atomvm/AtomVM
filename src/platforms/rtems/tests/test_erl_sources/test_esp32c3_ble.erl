%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M. <petermm@gmail.com>
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

-module(test_esp32c3_ble).
-export([start/0]).

% Credentials are supplied privately over UART using the HW_TEST.md protocol.
% Keep Wi-Fi associated while the minimal BLE controller advertises.
-define(ADVERTISE_SECONDS, 60).

start() ->
    erlang:display({atomvm_esp32c3_ble, atomvm:platform()}),
    {ok, Console} = uart:init([{peripheral, "/dev/console"}, {speed, 115200}, {raw, true}]),
    erlang:display({wifi_config, ready}),
    <<SsidLen, PskLen>> = read_exact(Console, 2, <<>>),
    true = SsidLen > 0 andalso SsidLen =< 32 andalso PskLen =< 63,
    Payload = read_exact(Console, SsidLen + PskLen, <<>>),
    <<Ssid:SsidLen/binary, Psk:PskLen/binary>> = Payload,
    ok = uart:deinit(Console),
    % Bluetooth must also work before esp_wifi_init().
    Parent = self(),
    spawn(fun() ->
        timer:sleep(100),
        Parent ! {bt_vm_progress, atomvm_rtems:bt_status()}
    end),
    ok = atomvm_rtems:bt_start(),
    receive
        {bt_vm_progress, starting} -> erlang:display({bt_vm_progress, ok})
    after 1000 -> erlang:error(bt_blocked_vm)
    end,
    erlang:display({bt_start, ok}),
    timer:sleep(10000),
    ok = atomvm_rtems:wifi_start(Ssid, Psk),
    ok = wait_wifi(120),
    coexist(?ADVERTISE_SECONDS div 10),
    ok = atomvm_rtems:wifi_stop(),
    erlang:display({wifi_stop_during_ble, ok}),
    timer:sleep(5000),
    ok = atomvm_rtems:wifi_start(Ssid, Psk),
    ok = wait_wifi(120),
    coexist(?ADVERTISE_SECONDS div 10),
    erlang:display({bt_test_complete, ok}),
    ok.

read_exact(_Console, 0, Acc) -> Acc;
read_exact(Console, Remaining, Acc) ->
    {ok, Data} = uart:read(Console, Remaining, 60000),
    read_exact(Console, Remaining - byte_size(Data), <<Acc/binary, Data/binary>>).

wait_wifi(0) -> erlang:error(wifi_timeout);
wait_wifi(N) ->
    {ok, Info} = atomvm_rtems:wifi_status(),
    case proplists:get_value(state, Info) of
        got_ip -> erlang:display({wifi_dhcp, ok}), ok;
        _ -> timer:sleep(500), wait_wifi(N - 1)
    end.

coexist(0) -> ok;
coexist(N) ->
    timer:sleep(5000),
    started = atomvm_rtems:bt_status(),
    {ok, Info} = atomvm_rtems:wifi_status(),
    got_ip = proplists:get_value(state, Info),
    {ok, Address} = inet:getaddr("example.com", inet),
    {ok, Sock} = socket:open(inet, stream, tcp),
    ok = socket:connect(Sock, #{family => inet, addr => Address, port => 80}),
    ok = socket:send(Sock, <<"GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n">>),
    {ok, <<"HTTP/">>} = socket:recv(Sock, 5, 10000),
    ok = socket:close(Sock),
    erlang:display({wifi_during_ble, N, ok}),
    coexist(N - 1).
