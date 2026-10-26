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

-module(test_peripherals).
-export([start/0]).

start() ->
    #{model := Model} = erlang:system_info(esp32_chip_info),
    erlang:display({peripherals, Model, start}),
    ok = esp:rtc_slow_set_binary(<<"peripheral retention">>),
    <<"peripheral retention">> = esp:rtc_slow_get_binary(),
    Before = atomvm_rtems:get_rtc_counter(),
    timer:sleep(30),
    After = atomvm_rtems:get_rtc_counter(),
    erlang:display({rtc_counter, Before, After}),
    true = After > Before,
    ok = ledc:timer_config([{duty_resolution, 12}, {freq_hz, 1000}, {speed_mode, 1}, {timer_num, 3}]),
    1000 = ledc:get_freq(1, 3),
    Channel = case Model of esp32_s31 -> 7; esp32_p4 -> 7; _ -> 5 end,
    ok = ledc:channel_config([{channel, Channel}, {duty, 4096}, {gpio_num, 3}, {speed_mode, 1}, {hpoint, 0}, {timer_sel, 3}]),
    timer:sleep(2),
    4096 = ledc:get_duty(1, Channel),
    ok = ledc:set_duty_and_update(1, Channel, 512, 0),
    timer:sleep(2),
    512 = ledc:get_duty(1, Channel),
    ok = ledc:stop(1, Channel, 0),
    erlang:display({pwm, Model, ok}),
    {ok, Unit} = esp_adc:init(),
    % P4 boot smoke covers ADC1; exercise the last ADC2 channel here.
    Pin = case Model of esp32_s31 -> 49; esp32_p4 -> 54; esp32_c5 -> 6; esp32_c6 -> 6; _ -> 4 end,
    {ok, ADC} = esp_adc:acquire(Pin, Unit, bit_12, db_11),
    {error, busy} = esp_adc:acquire(Pin, Unit, bit_12, db_11),
    {ok, OtherUnit} = esp_adc:init(),
    {'EXIT', {badarg, _}} = (catch esp_adc:sample(ADC, OtherUnit, [raw])),
    ok = esp_adc:deinit(OtherUnit),
    Sample = esp_adc:sample(ADC, Unit, [raw, {samples, 1}]),
    erlang:display({adc, Model, Sample}),
    case Sample of
        {ok, {Raw, undefined}} when Raw >= 0, Raw =< 4095 -> ok;
        {error, timeout} when Model =/= esp32_p4 -> ok
    end,
    ok = esp_adc:release_channel(ADC),
    {'EXIT', {badarg, _}} = (catch esp_adc:sample(ADC, Unit, [raw])),
    ok = esp_adc:deinit(Unit),
    ok = esp:sleep_enable_timer_wakeup(50000),
    Parent = self(),
    Workers = [spawn(fun() -> receive resume -> Parent ! {self(), resumed} end end) || _ <- lists:seq(1, 4)],
    Timer = erlang:send_after(20, self(), awake),
    T0 = erlang:monotonic_time(millisecond),
    erlang:display({hardware_sleep, Model, enter}),
    Result = esp:light_sleep(),
    T1 = erlang:monotonic_time(millisecond),
    erlang:display({hardware_sleep, Model, Result, T1 - T0, esp:sleep_get_wakeup_cause()}),
    ok = Result,
    true = T1 - T0 >= 40,
    sleep_wakeup_timer = esp:sleep_get_wakeup_cause(),
    % A timer can fire before the receiver is scheduled. Use the same
    % liveness budget as the workers, not a 20 ms scheduling guarantee.
    receive awake -> ok after 1000 -> erlang:error({timer_lost, Timer}) end,
    <<"peripheral retention">> = esp:rtc_slow_get_binary(),
    lists:foreach(fun(Pid) -> Pid ! resume end, Workers),
    lists:foreach(fun(Pid) -> receive {Pid, resumed} -> ok after 1000 -> erlang:error(cpu_did_not_resume) end end, Workers),
    ok = esp:sleep_enable_timer_wakeup(50000),
    ok = esp:light_sleep(),
    timer:sleep(20),
    ok.
