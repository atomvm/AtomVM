%
% This file is part of AtomVM.
%
% Copyright 2020 Fred Dushin <fred@dushin.net>
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

-module(deep_sleep).
-export([start/0]).

start() ->
    % If you want to reconnect over USB, this may help.
    io:format("Sleeping for 10 seconds to ease reconnection~n"),
    timer:sleep(10000),
    io:format("Wakeup cause: ~p~n", [esp:sleep_get_wakeup_cause()]),

    SystemInfo = erlang:system_info(esp32_chip_info),
    GPIOWakeUpMethod =
        case maps:get(model, SystemInfo) of
            esp32 ->
                ext0;
            esp32_s2 ->
                ext0;
            esp32_s3 ->
                ext0;
            esp32_h2 ->
                ext1;
            esp32_c2 ->
                gpio;
            esp32_c3 ->
                gpio;
            esp32_c6 ->
                gpio;
            esp32_p4 ->
                gpio;
            Other ->
                io:format("Not sure which GPIO wakeup API this SOC (~p) supports\n", [Other]),
                {unknown, Other}
        end,

    % In this example, we don't use internal pull downs, which means that you
    % should use an external one (this will eventually reduce power further).
    gpio:set_pin_mode(0, input),
    gpio:set_pin_pull(0, floating),

    case GPIOWakeUpMethod of
        ext0 ->
            ok = esp:sleep_enable_ext0_wakeup(0, 1),
            io:format("Using esp:sleep_enable_ext0_wakeup/2 to enable wake up on pin 0\n");
        ext1 ->
            ok = esp:sleep_enable_ext1_wakeup(1, 1),
            io:format("Using esp:sleep_enable_ext1_wakeup/2 to enable wake up on pin 0\n");
        gpio ->
            ok = esp:deep_sleep_enable_gpio_wakeup(1, 1),
            io:format("Using esp:deep_sleep_enable_gpio_wakeup/2 to enable wake up on pin 0\n");
        {unknown, SOC} ->
            io:format("Not sure what API this soc (~p) supports\n", [SOC])
    end,

    SleepMs = 5000,
    io:format("Deep sleeping for ~pms~n", [SleepMs]),
    esp:deep_sleep(SleepMs).
