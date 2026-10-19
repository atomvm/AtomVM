% Copyright 2026 Peter M. <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(test_wokwi_wifi).
-export([start/0]).
-define(TEST_WIFI_WOKWI, true).
-include("wifi_scan.hrl").

start() ->
    erlang:display({wokwi_wifi_start, atomvm:platform()}),
    wifi_scan_smoke(),
    wifi_scan_stop(),
    erlang:display({wifi_test_complete, ok}),
    ok.

wait_for_wifi(0) ->
    erlang:error({wifi_timeout, atomvm_rtems:wifi_status()});
wait_for_wifi(Attempts) ->
    {ok, Info} = atomvm_rtems:wifi_status(),
    case proplists:get_value(state, Info) of
        got_ip ->
            IP = proplists:get_value(ip, Info),
            true = is_tuple(IP) andalso tuple_size(IP) =:= 4 andalso IP =/= {0, 0, 0, 0},
            {ok, Addrs} = atomvm_rtems:ifaddrs(),
            true = lists:any(fun({_Name, Addr, _Flags}) -> Addr =:= IP end, Addrs),
            erlang:display({wifi_dhcp, IP, ok});
        _ ->
            timer:sleep(500),
            wait_for_wifi(Attempts - 1)
    end.

% Wokwi's public gateway can reach the Internet, not the host's HTTP fixture.
% Require real DNS and an HTTP response while the station remains connected.
wifi_scan_connected_traffic() ->
    {ok, IP} = inet:getaddr("example.com", inet),
    erlang:display({wifi_dns, IP, ok}),
    {ok, Sock} = socket:open(inet, stream, tcp),
    ok = socket:connect(Sock, #{family => inet, addr => IP, port => 80}),
    ok = socket:send(Sock, <<"HEAD / HTTP/1.0\r\nHost: example.com\r\n\r\n">>),
    {ok, <<"HTTP/", _/binary>>} = socket:recv(Sock, 0, 10000),
    ok = socket:close(Sock),
    erlang:display({wifi_scan, connected_traffic, ok}).
