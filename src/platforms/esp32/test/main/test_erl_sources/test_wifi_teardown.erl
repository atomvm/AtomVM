%
% This file is part of AtomVM.
%
% Copyright 2026 Davide Bettio <davide@uninstall.it>
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

-module(test_wifi_teardown).

-export([start/0]).

%% Deliberately start WiFi and return WITHOUT calling network:stop(): the
%% network port driver destroy callback must tear everything down when this
%% VM instance is destroyed, so that the next VM instance can start WiFi
%% again from scratch. The test harness runs this module twice for that
%% reason (see test_wifi_teardown in test_main.c).
start() ->
    case verify_platform(atomvm:platform()) of
        ok ->
            {ok, _Pid} = network:start([{sta, [managed]}]),
            %% sta_status is served only after the driver acknowledged the
            %% start command, so WiFi is really up once this call returns.
            disconnected = network:sta_status(),
            ok;
        Error ->
            Error
    end.

verify_platform(esp32) ->
    ok;
verify_platform(Platform) ->
    {error, {unsupported_platform, Platform}}.
