%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M. <petermm@gmail.com>
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

-module(test_deep_sleep).
-export([start/0]).

start() ->
    case esp:reset_reason() of
        esp_rst_deepsleep ->
            <<"deep sleep", Before:64>> = esp:rtc_slow_get_binary(),
            sleep_wakeup_timer = esp:sleep_get_wakeup_cause(),
            After = atomvm_rtems:get_rtc_counter(),
            true = After > Before,
            <<"deep sleep", Before:64>> = esp:rtc_slow_get_binary(),
            erlang:display({deep_sleep_retention, Before, After, ok}),
            ok;
        _ ->
            Before = atomvm_rtems:get_rtc_counter(),
            ok = esp:rtc_slow_set_binary(<<"deep sleep", Before:64>>),
            erlang:display({deep_sleep, enter}),
            Result = esp:deep_sleep(100),
            erlang:error({deep_sleep_returned, Result})
    end.
