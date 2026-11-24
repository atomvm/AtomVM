%
% This file is part of AtomVM.
%
% Copyright 2026 Gabriel Mancini <gabriel.mancini@gmail.com>
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

-module(test_eth).

-export([start/0]).

start() ->
    ok = test_badarg(),
    ok = test_wait_for_eth_refused(),
    ok = test_static_ip(),
    ok = test_callbacks_in_order(),
    ok = test_wait_for_eth_after_stop(),
    ok = test_sntp_alone(),
    ok = test_sntp_alone_badarg(),
    ok.

test_badarg() ->
    lists:foreach(fun expect_badarg/1, [
        [{phy_addr, 32}],
        [{mdc, -5}],
        [{mdc, 99}],
        [{power, not_a_pin}],
        [{rmii_clock, {out, 99}}],
        [{rmii_clock, {sideways, 0}}],
        [{route_priority, 256}],
        [{ipv4, invalid}],
        [{ipv4, {{10, 0, 2, 42}, {255, 255, 255, 0}}}],
        [{ipv4, {{10, 0, 42}, {255, 255, 255, 0}, {0, 0, 0, 0}}}],
        [{ipv4, {{10, 0, 2, -1}, {255, 255, 255, 0}, {0, 0, 0, 0}}}],
        [{ipv4, {{10, 0, 2, 42}, {255, 255, 255, 256}, {0, 0, 0, 0}}}],
        [{ipv4, {{10, 0, 2, 42}, {255, 255, 255, 0}, {0, 0, 0, 1 bsl 40}}}],
        [{ipv4, {{0, 0, 0, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}}}],
        [{ipv4, {{10, 0, 2, 42}, {0, 0, 0, 0}, {0, 0, 0, 0}}}]
    ]).

expect_badarg(EthConfig) ->
    {ok, Pid} = network:start([{eth, EthConfig}]),
    Monitor = monitor(process, Pid),
    receive
        {'DOWN', Monitor, process, Pid, {start_port_failed, badarg}} -> ok;
        {'DOWN', Monitor, process, Pid, Other} -> error({unexpected, EthConfig, Other})
    after 5000 -> error({timeout, EthConfig})
    end.

% SNTP alone, for an interface brought up outside the network driver:
% accepted, not refused as a config with no network. sta_status/0 is answered
% only once the driver has replied to the start.
test_sntp_alone() ->
    {ok, _Pid} = network:start([{sntp, [{host, <<"pool.ntp.org">>}]}]),
    inactive = network:sta_status(),
    ok = network:stop().

% SNTP alone that cannot start, with no host or a host that is not a string,
% is refused by the driver rather than accepted with nothing running.
test_sntp_alone_badarg() ->
    lists:foreach(fun expect_start_badarg/1, [
        [{sntp, []}],
        [{sntp, [{host, invalid}]}]
    ]).

expect_start_badarg(Config) ->
    {ok, Pid} = network:start(Config),
    Monitor = monitor(process, Pid),
    receive
        {'DOWN', Monitor, process, Pid, {start_port_failed, badarg}} -> ok;
        {'DOWN', Monitor, process, Pid, Other} -> error({unexpected, Config, Other})
    after 5000 -> error({timeout, Config})
    end.

%% wait_for_eth/2 returns the driver's reason, not a timeout, and leaves no
%% monitor message behind.
%% A fixed address is reported as given, by got_ip, on two start/stop cycles.
%% wait_for_eth/2 leaves its connected and disconnected callbacks pointed at
%% this process: whatever they sent is flushed after each cycle, or the next
%% test would read it as its own first event.
test_static_ip() ->
    IpInfo = {{10, 0, 2, 42}, {255, 255, 255, 0}, {0, 0, 0, 0}},
    lists:foreach(
        fun(_) ->
            {ok, IpInfo} = network:wait_for_eth([{ipv4, IpInfo}], 5000),
            connected = network:eth_status(),
            ok = network:stop(),
            flush()
        end,
        [first, second]
    ).

test_wait_for_eth_refused() ->
    {error, badarg} = network:wait_for_eth([{phy_addr, 32}], 5000),
    receive
        {'DOWN', _, process, _, _} = Down -> error({stale, Down})
    after 0 -> ok
    end.

%% Pid callbacks are sent from the network gen_server itself, so they arrive in
%% the order of the driver events.
test_callbacks_in_order() ->
    inactive = eth_status_or_inactive(),
    Self = self(),
    {ok, _Pid} = network:start([
        {eth, [
            {started, Self},
            {connected, Self},
            {got_ip, Self},
            {disconnected, Self}
        ]}
    ]),
    ok = expect(started),
    ok = expect(connected),
    {got_ip, {_Ip, _Netmask, _Gw}} = expect_got_ip(),
    connected = network:eth_status(),
    ok = network:stop(),
    flush(),
    ok.

%% network:stop/0 must leave the EMAC, netif and handlers as before
%% network:start/1, so a second start gets an address again.
test_wait_for_eth_after_stop() ->
    {ok, {_Ip, _Netmask, _Gw}} = network:wait_for_eth(),
    connected = network:eth_status(),
    ok = network:stop(),
    ok.

eth_status_or_inactive() ->
    case whereis(network) of
        undefined -> inactive;
        _Pid -> network:eth_status()
    end.

expect(Message) ->
    receive
        Message -> ok;
        Other -> {unexpected, Message, Other}
    after 10000 -> {timeout, Message}
    end.

expect_got_ip() ->
    receive
        {got_ip, _IpInfo} = GotIp -> GotIp;
        Other -> {unexpected, got_ip, Other}
    after 10000 -> {timeout, got_ip}
    end.

flush() ->
    receive
        _ -> flush()
    after 0 -> ok
    end.
