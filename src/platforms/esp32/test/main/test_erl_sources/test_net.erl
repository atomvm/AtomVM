%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_net).
-export([start/0]).

start() ->
    ok = test_service_undefined(),
    ok = test_service_https(),
    ok = test_invalid_hostname(),
    ok.

test_service_undefined() ->
    % Get address of github.com
    {ok, Results} = net:getaddrinfo_nif("github.com", undefined),
    % We should have at least one UDP and one TCP IPv4 entry
    [_TCPAddr | _] = [
        Addr
     || #{addr := Addr, type := stream, protocol := tcp, family := inet} <- Results
    ],
    [_UDPAddr | _] = [
        Addr
     || #{addr := Addr, type := dgram, protocol := udp, family := inet} <- Results
    ],
    ok.

test_service_https() ->
    {ok, Results} = net:getaddrinfo_nif("github.com", "https"),
    [_TCPAddr | _] = [
        Addr
     || #{addr := Addr, type := stream, protocol := tcp, family := inet} <- Results
    ],
    [_UDPAddr | _] = [
        Addr
     || #{addr := Addr, type := dgram, protocol := udp, family := inet} <- Results
    ],
    ok.

test_invalid_hostname() ->
    {error, eaifail} = net:getaddrinfo_nif("atomvm.invalid", undefined),
    ok.
