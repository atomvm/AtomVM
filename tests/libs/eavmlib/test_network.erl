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

-module(test_network).

-export([test/0]).

%% network:sta_rssi/0 against a fake network_port: it takes only the reply to
%% its own request, and gives up after 5 s.
test() ->
    ok = test_sta_rssi_network_down(),
    ok = test_sta_rssi_leaves_other_messages(),
    ok = test_sta_rssi_ignores_other_refs(),
    ok = test_sta_rssi_error(),
    ok = test_sta_rssi_timeout(),
    ok.

test_sta_rssi_network_down() ->
    undefined = whereis(network_port),
    {error, network_down} = network:sta_rssi(),
    ok.

test_sta_rssi_leaves_other_messages() ->
    self() ! unrelated,
    {ok, -42} = with_port(fun(From, Ref) -> From ! {Ref, {rssi, -42}} end),
    ok =
        receive
            unrelated -> ok
        after 0 -> lost
        end,
    ok.

test_sta_rssi_ignores_other_refs() ->
    Stale = make_ref(),
    {ok, -42} = with_port(fun(From, Ref) ->
        From ! {Stale, {rssi, -1}},
        From ! {Ref, {rssi, -42}}
    end),
    ok =
        receive
            {Stale, {rssi, -1}} -> ok
        after 0 -> lost
        end,
    ok.

test_sta_rssi_error() ->
    {error, network_down} = with_port(fun(From, Ref) -> From ! {Ref, {error, network_down}} end),
    ok.

test_sta_rssi_timeout() ->
    {error, timeout} = with_port(fun(_From, _Ref) -> ok end),
    ok.

with_port(Answer) ->
    Port = spawn(fun() ->
        receive
            {From, Ref, rssi} -> Answer(From, Ref)
        end,
        receive
            stop -> ok
        end
    end),
    true = register(network_port, Port),
    try
        network:sta_rssi()
    after
        unregister(network_port),
        Port ! stop
    end.
