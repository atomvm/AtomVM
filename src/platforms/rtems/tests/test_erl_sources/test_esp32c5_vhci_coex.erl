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

-module(test_esp32c5_vhci_coex).
-export([start/0]).

% Credentials are supplied privately over UART using the HW_TEST.md protocol.
% Keep Wi-Fi associated while the minimal BLE controller advertises.
-define(ADVERTISE_SECONDS, 60).
-ifndef(VHCI_HTTP_HOST).
-define(VHCI_HTTP_HOST, "example.com").
-endif.
-ifndef(VHCI_HTTP_PORT).
-define(VHCI_HTTP_PORT, 80).
-endif.

start() ->
    erlang:display({atomvm_esp32c5_vhci_coex, atomvm:platform()}),
    {ok, Console} = uart:init([{peripheral, "/dev/console"}, {speed, 115200}, {raw, true}]),
    erlang:display({wifi_config, ready}),
    <<SsidLen, PskLen>> = read_exact(Console, 2, <<>>),
    true = SsidLen > 0 andalso SsidLen =< 32 andalso PskLen =< 63,
    Payload = read_exact(Console, SsidLen + PskLen, <<>>),
    <<Ssid:SsidLen/binary, Psk:PskLen/binary>> = Payload,
    ok = uart:deinit(Console),
    % Bluetooth must also work before esp_wifi_init().
    ok = start_vhci(),
    erlang:display({bt_start, ok}),
    timer:sleep(10000),
    ok = atomvm_rtems:wifi_start(Ssid, Psk),
    ok = wait_wifi(120),
    coexist(?ADVERTISE_SECONDS div 5),
    ok = atomvm_rtems:wifi_stop(),
    erlang:display({wifi_stop_during_ble, ok}),
    timer:sleep(5000),
    ok = atomvm_rtems:wifi_start(Ssid, Psk),
    ok = wait_wifi(120),
    coexist(?ADVERTISE_SECONDS div 5),
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
    {ok, Address} = inet:getaddr(?VHCI_HTTP_HOST, inet),
    {ok, Sock} = socket:open(inet, stream, tcp),
    ok = socket:connect(Sock, #{family => inet, addr => Address, port => ?VHCI_HTTP_PORT}),
    ok = socket:send(Sock, <<"GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n">>),
    {ok, <<"HTTP/">>} = socket:recv(Sock, 5, 10000),
    ok = socket:close(Sock),
    erlang:display({wifi_during_ble, N, ok}),
    coexist(N - 1).

start_vhci() ->
    Parent = self(),
    spawn_link(fun() ->
        {ok, First} = reopen(100),
        ok = ble_vhci:close(First),
        {ok, Handle} = reopen(100),
        lists:foreach(fun(Packet) -> command(Handle, Packet) end, [
            <<1, 3, 12, 0>>, % HCI Reset
            <<1, 1, 12, 8, 0, 0, 0, 0, 0, 0, 0, 32>>, % Set Event Mask
            <<1, 1, 32, 8, 0, 0, 2, 0, 0, 0, 0, 0>>, % LE Set Event Mask
            % LE Set Extended Advertising Parameters: same profile as the smoke test.
            <<1, 54, 32, 25, 0, 16, 0, 0, 8, 0, 0, 8, 0, 7, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 127, 1, 0, 1, 0, 0>>
        ]),
        Parent ! vhci_advertising_ready,
        advertise(Handle, 0)
    end),
    receive vhci_advertising_ready -> ok after 10000 -> erlang:error(vhci_init_timeout) end.

reopen(0) -> erlang:error(vhci_reopen_timeout);
reopen(N) ->
    case ble_vhci:init() of
        {error, already_started} -> timer:sleep(10), reopen(N - 1);
        Result -> Result
    end.

command(Handle, <<1, Opcode:16/little, _/binary>> = Packet) ->
    ok = ble_vhci:send(Handle, self(), Packet),
    receive
        {hci_event, <<4, 14, _Length, _Credits, Opcode:16/little, 0, _/binary>>} -> ok
    after 5000 -> erlang:error({hci_timeout, Opcode}) end.

advertise(Handle, Counter) ->
    % LE Set Extended Advertising Data: AtomVM-BLE and a manufacturer counter.
    command(Handle, <<1, 55, 32, 27, 0, 3, 1, 23, 2, 1, 6, 11, 9,
        65, 116, 111, 109, 86, 77, 45, 66, 76, 69, 7, 255, 255, 255, Counter:32/little>>),
    % LE Set Extended Advertising Enable: one advertising event.
    command(Handle, <<1, 57, 32, 6, 1, 1, 0, 0, 0, 1>>),
    receive
        {hci_event, <<4, 62, 6, 18, 67, 0, _Conn:16/little, 1>>} -> ok
    after 5000 -> erlang:error(advertising_event_timeout) end,
    erlang:display({vhci_counter, Counter}),
    timer:sleep(1280),
    advertise(Handle, Counter + 1).
