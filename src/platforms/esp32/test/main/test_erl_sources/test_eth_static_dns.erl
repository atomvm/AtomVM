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

-module(test_eth_static_dns).

-export([start/0]).

%% Start Ethernet with a fixed address and no DHCP. The test harness seeds the
%% DNS servers before and checks them after (see test_eth_static_dns in
%% test_main.c): lwIP shares them with every interface, so a fixed address
%% must not clear them.
start() ->
    IpInfo = {{10, 0, 2, 42}, {255, 255, 255, 0}, {0, 0, 0, 0}},
    {ok, IpInfo} = network:wait_for_eth([{ipv4, IpInfo}], 5000),
    connected = network:eth_status(),
    ok = network:stop().
