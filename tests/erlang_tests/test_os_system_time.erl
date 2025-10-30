%
% This file is part of AtomVM.
%
% Copyright 2018-2023 Davide Bettio <davide@uninstall.it>
% Copyright 2025 Mateusz Furga <mateusz.furga@swmansion.com>
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

-module(test_os_system_time).

-export([start/0]).

start() ->
    ok = test_os_system_time(101),
    ok = test_os_system_time(10),
    ok = test_os_system_time(1),
    0.

test_os_system_time(SleepMs) ->
    Before = os:system_time(),
    sleep(SleepMs),
    After = os:system_time(),
    true = (Before < After),
    ok.

sleep(Ms) ->
    receive
    after Ms ->
        ok
    end.
