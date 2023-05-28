%
% This file is part of AtomVM.
%
% Copyright 2019-2023 Fred Dushin <fred@dushin.net>
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

%%-----------------------------------------------------------------------------
%% @doc network_fsm
%%
%% This module is depreciated. Use the network module instead.
%% @end
%%-----------------------------------------------------------------------------
-module(network_fsm).

-export([
    wait_for_sta/0, wait_for_sta/1, wait_for_sta/2,
    wait_for_ap/0, wait_for_ap/1, wait_for_ap/2
]).
-export([start/1, stop/0]).

wait_for_sta() ->
    io:format(
        "WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"
    ),
    network:wait_for_sta().

wait_for_sta(StaConfig) ->
    io:format(
        "WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"
    ),
    network:wait_for_sta(StaConfig).

wait_for_sta(StaConfig, Timeout) ->
    io:format(
        "WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"
    ),
    network:wait_for_sta(StaConfig, Timeout).

wait_for_ap() ->
    io:format(
        "WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"
    ),
    network:wait_for_ap().

wait_for_ap(ApConfig) ->
    io:format(
        "WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"
    ),
    network:wait_for_ap(ApConfig).

wait_for_ap(ApConfig, Timeout) ->
    io:format(
        "WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"
    ),
    network:wait_for_ap(ApConfig, Timeout).

start(Config) ->
    io:format(
        "WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"
    ),
    network:start(Config).

stop() ->
    io:format(
        "WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"
    ),
    network:stop().
