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

-module(ledc_example).
-export([start/0]).

-include("ledc.hrl").

-define(LEDC_HS_TIMER, ?LEDC_TIMER_0).
-define(LEDC_HS_MODE, ?LEDC_HIGH_SPEED_MODE).
-define(LEDC_HS_CH0_GPIO, 18).
-define(LEDC_HS_CH0_CHANNEL, ?LEDC_CHANNEL_0).
-define(LEDC_HS_CH1_GPIO, 19).
-define(LEDC_HS_CH1_CHANNEL, ?LEDC_CHANNEL_1).

-define(LEDC_LS_TIMER, ?LEDC_TIMER_1).
-define(LEDC_LS_MODE, ?LEDC_LOW_SPEED_MODE).
-define(LEDC_LS_CH2_GPIO, 4).
-define(LEDC_LS_CH2_CHANNEL, ?LEDC_CHANNEL_2).
-define(LEDC_LS_CH3_GPIO, 5).
-define(LEDC_LS_CH3_CHANNEL, ?LEDC_CHANNEL_3).

-define(LEDC_TEST_DUTY, 4000).
-define(LEDC_TEST_FADE_TIME, 3000).

start() ->
    LEDCHSTimer = [
        {duty_resolution, ?LEDC_TIMER_13_BIT},
        {freq_hz, 5000},
        {speed_mode, ?LEDC_HS_MODE},
        {timer_num, ?LEDC_HS_TIMER}
    ],
    ok = ledc:timer_config(LEDCHSTimer),
    LEDCLSTimer = [
        {duty_resolution, ?LEDC_TIMER_13_BIT},
        {freq_hz, 5000},
        {speed_mode, ?LEDC_LS_MODE},
        {timer_num, ?LEDC_LS_TIMER}
    ],
    ok = ledc:timer_config(LEDCLSTimer),
    LEDCChannel = [
        [
            {channel, ?LEDC_HS_CH0_CHANNEL},
            {duty, 0},
            {gpio_num, ?LEDC_HS_CH0_GPIO},
            {speed_mode, ?LEDC_HS_MODE},
            {hpoint, 0},
            {timer_sel, ?LEDC_HS_TIMER}
        ],
        [
            {channel, ?LEDC_HS_CH1_CHANNEL},
            {duty, 0},
            {gpio_num, ?LEDC_HS_CH1_GPIO},
            {speed_mode, ?LEDC_HS_MODE},
            {hpoint, 0},
            {timer_sel, ?LEDC_HS_TIMER}
        ],
        [
            {channel, ?LEDC_LS_CH2_CHANNEL},
            {duty, 0},
            {gpio_num, ?LEDC_LS_CH2_GPIO},
            {speed_mode, ?LEDC_LS_MODE},
            {hpoint, 0},
            {timer_sel, ?LEDC_LS_TIMER}
        ],
        [
            {channel, ?LEDC_LS_CH3_CHANNEL},
            {duty, 0},
            {gpio_num, ?LEDC_LS_CH3_GPIO},
            {speed_mode, ?LEDC_LS_MODE},
            {hpoint, 0},
            {timer_sel, ?LEDC_LS_TIMER}
        ]
    ],
    lists:foreach(
        fun(ChannelConfig) ->
            ok = ledc:channel_config(ChannelConfig)
        end,
        LEDCChannel
    ),
    ok = ledc:fade_func_install(0),
    loop(LEDCChannel).

loop(LEDCChannel) ->
    io:format("1. LEDC fade up to duty = ~p~n", [?LEDC_TEST_DUTY]),
    lists:foreach(
        fun(ChannelConfig) ->
            SpeedMode = proplists:get_value(speed_mode, ChannelConfig),
            Channel = proplists:get_value(channel, ChannelConfig),
            ok = ledc:set_fade_with_time(SpeedMode, Channel, ?LEDC_TEST_DUTY, ?LEDC_TEST_FADE_TIME),
            ok = ledc:fade_start(SpeedMode, Channel, ?LEDC_FADE_NO_WAIT)
        end,
        LEDCChannel
    ),
    timer:sleep(?LEDC_TEST_FADE_TIME),

    io:format("2. LEDC fade down to duty = 0~n"),
    lists:foreach(
        fun(ChannelConfig) ->
            SpeedMode = proplists:get_value(speed_mode, ChannelConfig),
            Channel = proplists:get_value(channel, ChannelConfig),
            ok = ledc:set_fade_with_time(SpeedMode, Channel, 0, ?LEDC_TEST_FADE_TIME),
            ok = ledc:fade_start(SpeedMode, Channel, ?LEDC_FADE_NO_WAIT)
        end,
        LEDCChannel
    ),
    timer:sleep(?LEDC_TEST_FADE_TIME),

    io:format("3. LEDC set duty = ~p without fade~n", [?LEDC_TEST_DUTY]),
    lists:foreach(
        fun(ChannelConfig) ->
            SpeedMode = proplists:get_value(speed_mode, ChannelConfig),
            Channel = proplists:get_value(channel, ChannelConfig),
            ok = ledc:set_duty(SpeedMode, Channel, ?LEDC_TEST_DUTY),
            ok = ledc:update_duty(SpeedMode, Channel)
        end,
        LEDCChannel
    ),
    timer:sleep(?LEDC_TEST_FADE_TIME),

    io:format("4. LEDC set duty = 0 without fade~n"),
    lists:foreach(
        fun(ChannelConfig) ->
            SpeedMode = proplists:get_value(speed_mode, ChannelConfig),
            Channel = proplists:get_value(channel, ChannelConfig),
            ok = ledc:set_duty(SpeedMode, Channel, 0),
            ok = ledc:update_duty(SpeedMode, Channel)
        end,
        LEDCChannel
    ),
    timer:sleep(?LEDC_TEST_FADE_TIME),

    loop(LEDCChannel).
