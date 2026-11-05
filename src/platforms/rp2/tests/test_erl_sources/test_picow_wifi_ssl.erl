%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
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

-module(test_picow_wifi_ssl).

-export([start/0]).

-define(HOST, "test.atomvm.org").
-define(HTTPS_PORT, 443).
-define(GOT_IP_TIMEOUT, 60000).
-define(SNTP_TIMEOUT, 180000).
-define(MBEDTLS_ERR_X509_CERT_VERIFY_FAILED, -16#2700).

start() ->
    try run() of
        ok ->
            ok
    catch
        Class:Reason:Stacktrace ->
            io:format("PICO_SIMTEST_FAIL ~p:~p~n", [Class, Reason]),
            erlang:raise(Class, Reason, Stacktrace)
    end.

run() ->
    Self = self(),
    Config = [
        {sta, [
            {ssid, <<"Wokwi-GUEST">>},
            % An absent PSK selects the open authentication required by Wokwi-GUEST on RP2.
            {got_ip, fun(IpInfo) -> Self ! {got_ip, IpInfo} end}
        ]},
        {sntp, [
            {host, "pool.ntp.org"},
            {synchronized, fun(TimeVal) -> Self ! {sntp_sync, TimeVal} end}
        ]}
    ],
    {ok, _Pid} = network:start(Config),
    try
        {Seconds, Microseconds} = wait_for_network(),
        ok = ssl:start(),
        try
            ok = test_certificate_time_check(Seconds, Microseconds),
            ok = test_verified_https()
        after
            ok = ssl:stop()
        end
    after
        ok = network:stop()
    end.

wait_for_network() ->
    receive
        {got_ip, IpInfo} ->
            io:format("PICO_SIMTEST got_ip ~p~n", [IpInfo])
    after ?GOT_IP_TIMEOUT ->
        error(got_ip_timeout)
    end,
    receive
        {sntp_sync, {Seconds, Microseconds}} when Seconds > 1704067200 ->
            io:format("PICO_SIMTEST sntp ~p~n", [Seconds]),
            {Seconds, Microseconds}
    after ?SNTP_TIMEOUT ->
        error(sntp_timeout)
    end.

test_certificate_time_check(Seconds, Microseconds) ->
    ok = atomvm:posix_clock_settime(realtime, {0, 0}),
    case verified_connect() of
        {error, ?MBEDTLS_ERR_X509_CERT_VERIFY_FAILED} ->
            io:format("PICO_SIMTEST epoch_rejected~n");
        {error, UnexpectedReason} ->
            error({unexpected_epoch_tls_error, UnexpectedReason});
        {ok, UnexpectedSocket} ->
            _ = ssl:close(UnexpectedSocket),
            error(certificate_time_not_checked)
    end,
    % SNTP reports microseconds, while posix_clock_settime/2 accepts nanoseconds.
    ok = atomvm:posix_clock_settime(realtime, {Seconds, Microseconds * 1000}),
    ok.

test_verified_https() ->
    {ok, Socket} = verified_connect(),
    try
        ok = ssl:send(
            Socket,
            <<"HEAD / HTTP/1.1\r\nHost: test.atomvm.org\r\nConnection: close\r\n\r\n">>
        ),
        {ok, <<"HTTP/1.1 200 OK">>} = ssl:recv(Socket, 15),
        ok
    after
        _ = ssl:close(Socket)
    end,
    io:format("PICO_SIMTEST verified_https~n"),
    ok.

verified_connect() ->
    ssl:connect(?HOST, ?HTTPS_PORT, [
        {verify, verify_peer},
        {cacerts, [isrg_root_x1()]},
        {active, false},
        {binary, true}
    ]).

% Official ISRG Root X1 from https://letsencrypt.org/certs/isrgrootx1.pem
isrg_root_x1() ->
    <<
        "-----BEGIN CERTIFICATE-----\n"
        "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\n"
        "-----END CERTIFICATE-----\n"
    >>.
