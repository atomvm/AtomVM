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

%% The QEMU harness emulates an OpenCores Ethernet MAC: the network driver
%% runs it with a DP83848 PHY at address 1 (see start_eth in network_driver.c).
start() ->
    ok = test_badarg(),
    ok = test_callbacks_in_order(),
    ok = test_wait_for_eth_after_stop(),
    ok.

%% The driver refuses invalid pins and addresses before touching the hardware;
%% network then stops with the reason, after the port cleaned up.
test_badarg() ->
    lists:foreach(fun expect_badarg/1, [
        [{phy_addr, 32}],
        [{mdc, -5}],
        [{power, not_a_pin}],
        [{rmii_clock, {out, 5}}],
        [{rmii_clock, {sideways, 0}}]
    ]).

expect_badarg(EthConfig) ->
    {ok, Pid} = network:start([{eth, EthConfig}]),
    Monitor = monitor(process, Pid),
    receive
        {'DOWN', Monitor, process, Pid, {start_port_failed, badarg}} -> ok;
        {'DOWN', Monitor, process, Pid, Other} -> error({unexpected, EthConfig, Other})
    after 5000 -> error({timeout, EthConfig})
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
