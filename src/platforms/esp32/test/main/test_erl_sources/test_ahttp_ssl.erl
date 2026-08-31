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

%% Exercise HTTPS connections with `{verify, verify_none}', `{verify, verify_peer}'
%% (no CA), and `{verify, verify_peer}' + `{cacerts, crt_bundle}', including a
%% trusted chain with the wrong verification name.
-module(test_ahttp_ssl).

-export([start/0]).

-define(HOST, "test.atomvm.org").
-define(HTTPS_PORT, 443).

start() ->
    case verify_platform(atomvm:platform()) of
        ok ->
            try
                ok = start_network(),
                wait_for_clock(40),
                {{Year, _, _}, _} = erlang:universaltime(),
                io:format("clock year=~p~n", [Year]),
                ok = run_cases()
            after
                ok = network:stop()
            end;
        Error ->
            Error
    end.

start_network() ->
    Self = self(),
    Config = [
        {sta, [
            {ssid, "Wokwi-GUEST"},
            {psk, ""},
            {got_ip, fun(IpInfo) -> Self ! {got_ip, IpInfo} end}
        ]},
        {sntp, [{host, "pool.ntp.org"}, {synchronized, fun sntp_synchronized/1}]}
    ],
    case network:start(Config) of
        {ok, _Pid} ->
            receive
                {got_ip, {Address, Netmask, Gateway}} ->
                    io:format(
                        "Acquired IP address: ~p Netmask: ~p Gateway: ~p~n",
                        [Address, Netmask, Gateway]
                    ),
                    ok
            after 30000 ->
                error({unable_to_start_network, timeout})
            end;
        Error ->
            error({unable_to_start_network, Error})
    end.

run_cases() ->
    Cases = [
        {verify_none, [{verify, verify_none}], success},
        {verify_peer_no_ca, [{verify, verify_peer}], failure},
        {verify_peer_crt_bundle, [{verify, verify_peer}, {cacerts, crt_bundle}], success},
        {verify_peer_wrong_name,
            [
                {verify, verify_peer},
                {cacerts, crt_bundle},
                {server_name_indication, "wrong.example"}
            ],
            failure}
    ],
    lists:foreach(fun run_case/1, Cases),
    ok.

run_case({Label, SslOpts, Expected}) ->
    io:format("=== ~p ===~n", [Label]),
    ssl:start(),
    T0 = erlang:monotonic_time(millisecond),
    Result = ahttp_client:connect(https, ?HOST, ?HTTPS_PORT, [{active, false} | SslOpts]),
    T1 = erlang:monotonic_time(millisecond),
    io:format("connect ~p ms: ~p~n", [T1 - T0, fmt_conn(Result)]),
    assert_case(Expected, Result).

assert_case(success, {ok, Conn}) ->
    ok = ahttp_client:close(Conn);
assert_case(failure, {error, _Reason}) ->
    ok;
assert_case(Expected, Result) ->
    error({unexpected_result, Expected, fmt_conn(Result)}).

fmt_conn({ok, _Conn}) ->
    ok;
fmt_conn(Other) ->
    Other.

wait_for_clock(0) ->
    error(clock_not_synchronized);
wait_for_clock(N) ->
    case erlang:universaltime() of
        {{1970, _, _}, _} ->
            timer:sleep(500),
            wait_for_clock(N - 1);
        _ ->
            ok
    end.

sntp_synchronized({TVSec, TVUsec}) ->
    io:format("Synchronized time with SNTP server. TVSec=~p TVUsec=~p~n", [TVSec, TVUsec]).

verify_platform(esp32) ->
    ok;
verify_platform(Platform) ->
    {error, {unsupported_platform, Platform}}.
