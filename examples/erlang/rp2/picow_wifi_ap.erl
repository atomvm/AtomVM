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

-module(picow_wifi_ap).

-export([start/0]).

start() ->
    Config = [
        {ap, [
            %% If an SSID is not specified, AtomVM will default to atomvm-<hexmac>
            %% where <hexmac> is the hexadecimal representation of the factory-supplied
            %% MAC address of the Pico-W device.
            %% If a password is not specified, the AP network will be open, with
            %% no encryption or authentication (strongly discouraged)
            {ap_started, fun ap_started/0},
            {sta_connected, fun sta_connected/1},
            {sta_ip_assigned, fun sta_ip_assigned/1},
            {sta_disconnected, fun sta_disconnected/1}
        ]}
    ],
    case network:start(Config) of
        {ok, _Pid} ->
            timer:sleep(infinity);
        Error ->
            erlang:display(Error)
    end.

ap_started() ->
    io:format("AP started.\n").

sta_connected(Mac) ->
    io:format("STA connected with mac ~w\n", [Mac]).

sta_disconnected(Mac) ->
    io:format("STA disconnected with mac ~w\n", [Mac]).

sta_ip_assigned(Address) ->
    io:format("STA assigned address ~p\n", [Address]).
