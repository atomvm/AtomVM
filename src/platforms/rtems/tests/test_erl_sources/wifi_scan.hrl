% Copyright 2026 Peter M. <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

wifi_scan_smoke() ->
    {ok, _} = network:start([
        {sta, [
            {managed, true},
            {scan_dwell_ms, 360},
            {scan_passive, true},
            {disconnected, fun() -> ok end}
        ]}
    ]),
    disconnected = network:sta_status(),
    {error, network_down} = network:sta_rssi(),
    {error, badarg} = network:wifi_scan([{results, 0}]),
    {error, badarg} = network:wifi_scan([{results, 65}]),
    {error, badarg} = network:wifi_scan([{passive, invalid}]),
    {error, badarg} = network:wifi_scan([{dwell, 0}]),
    {ok, Active} = network:wifi_scan([{results, 1}, {dwell, 120}]),
    erlang:display({wifi_scan_active, Active}),
    check_scan_result(Active),
    {ok, Passive} = network:wifi_scan(),
    erlang:display({wifi_scan_passive, Passive}),
    case Passive of
        {0, []} -> ok;
        _ -> check_scan_result(Passive)
    end,
    erlang:display({wifi_scan, active_and_passive, ok}),
    erlang:garbage_collect(),
    erlang:garbage_collect(whereis(network)),

    ok = network:sta_connect([
        {sta, [{ssid, "AtomVM-TestAP"}, {psk, "secretpassword"}, {scan_done, self()}]}
    ]),
    wait_for_wifi(30),
    {ok, Rssi} = network:sta_rssi(),
    -30 = Rssi,
    erlang:display({wifi_rssi, Rssi, ok}),

    ok = network:wifi_scan([{dwell, 120}]),
    receive
        {scan_results, Async} -> check_scan_result(Async)
    after 15000 ->
        erlang:error(scan_callback_timeout)
    end,
    erlang:garbage_collect(),
    erlang:garbage_collect(whereis(network)),
    connected = network:sta_status(),
    wifi_scan_connected_traffic().

-ifdef(TEST_WIFI_SCAN_TRAFFIC).
wifi_scan_connected_traffic() ->
    {ok, Sock} = socket:open(inet, stream, tcp),
    ok = socket:connect(Sock, #{family => inet, addr => {192, 168, 4, 1}, port => 8080}),
    ok = socket:send(Sock, <<"HEAD /README.md HTTP/1.0\r\nHost: 192.168.4.1\r\n\r\n">>),
    {ok, Data} = socket:recv(Sock, 0, 5000),
    true = byte_size(Data) > 0,
    ok = socket:close(Sock),
    erlang:display({wifi_scan, connected_traffic, ok}).
-else.
% esp-emu 0.49 resets the virtual AP association after a scan on C5/C6/S31.
% These targets test traffic after restarting the station instead.
wifi_scan_connected_traffic() ->
    ok.
-endif.

wifi_scan_stop() ->
    ok = network:wifi_scan([{dwell, 1500}]),
    {error, busy} = network:wifi_scan([{dwell, 30}]),
    ok = network:stop(),
    receive
        {scan_results, {error, scan_canceled}} -> ok
    after 5000 ->
        erlang:error(scan_cancel_timeout)
    end,
    {error, network_down} = network:sta_rssi(),
    erlang:display({wifi_scan, callback_and_cancel, ok}).

check_scan_result({Found, Networks}) when is_integer(Found), Found > 0 ->
    true = length(Networks) > 0,
    [AP | _] = Networks,
    #{
        ssid := <<"AtomVM-TestAP">>,
        bssid := Bssid,
        rssi := Rssi,
        channel := Channel,
        authmode := Auth,
        hidden := false
    } = AP,
    6 = byte_size(Bssid),
    true = is_integer(Rssi) andalso Rssi >= -127 andalso Rssi =< 127,
    true = is_integer(Channel) andalso Channel > 0,
    true = is_atom(Auth) andalso Auth =/= undefined,
    ok.
