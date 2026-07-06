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
    ok = test_service_numeric(),
    ok = test_invalid_hostname(),
    ok.

test_service_undefined() ->
    % Get address of github.com
    {ok, Results} = net:getaddrinfo_nif("github.com", undefined),
    io:format("getaddrinfo Results: ~p~n", [Results]),
    % We should have at least one IPv4 entry
    [_SomeAddr | _] = [
        Addr
     || #{addr := Addr, family := inet} <- Results
    ],
    ok.

test_service_numeric() ->
    {ok, Results} = net:getaddrinfo_nif("github.com", "443"),
    % We should have at least one IPv4 entry
    [_SomeAddr | _] = [
        Addr
     || #{addr := Addr, family := inet} <- Results
    ],
    ok.

test_invalid_hostname() ->
    case net:getaddrinfo_nif("atomvm.invalid", undefined) of
        {error, eaifail} -> ok;
        {error, eainoname} -> ok;
        {error, eaiagain} -> ok;
        {error, -11} -> ok;
        {error, Reason} ->
            io:format("getaddrinfo failed with unexpected reason: ~p~n", [Reason]),
            ok
    end.
