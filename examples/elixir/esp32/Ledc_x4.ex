#
# This file is part of AtomVM.
#
# Copyright 2022 Winford (Uncle Grumpy) <dwinford@proton.me>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

defmodule Ledc_x4 do
  # this compiler option is to suppress warnings when compiling the VM
  # it is not needed or recommended for user apps.
  @compile {:no_warn_undefined, [LEDC]}
  @moduledoc """
  Ledc_example for Elixir.
  """

  @test_duty 4000
  @test_fade_time 3000

  @high_speed_timer 0
  @low_speed_timer 1

  @led_1 18
  @led_2 19
  @led_3 4
  @led_4 5

  def start do
    ledc_hs_timer = [
      {:duty_resolution, 13},
      {:freq_hz, 5000},
      {:speed_mode,  LEDC.high_speed_mode},
      {:timer_num, @high_speed_timer}
    ]

    :ok = LEDC.timer_config(ledc_hs_timer)

    ledc_ls_timer = [
      {:duty_resolution, 13},
      {:freq_hz, 5000},
      {:speed_mode,  LEDC.low_speed_mode},
      {:timer_num, @low_speed_timer}
    ]

    :ok = LEDC.timer_config(ledc_ls_timer)

    ledc_channel = [
      [
        {:channel, 0},
        {:duty, 0},
        {:gpio_num, @led_1},
        {:speed_mode,  LEDC.high_speed_mode},
        {:hpoint, 0},
        {:timer_sel, @high_speed_timer}
      ],
      [
        {:channel, 1},
        {:duty, 0},
        {:gpio_num, @led_2},
        {:speed_mode,  LEDC.high_speed_mode},
        {:hpoint, 0},
        {:timer_sel, @high_speed_timer}
      ],
      [
        {:channel, 2},
        {:duty, 0},
        {:gpio_num, @led_3},
        {:speed_mode,  LEDC.low_speed_mode},
        {:hpoint, 0},
        {:timer_sel, @low_speed_timer}
      ],
      [
        {:channel, 3},
        {:duty, 0},
        {:gpio_num, @led_4},
        {:speed_mode,  LEDC.low_speed_mode},
        {:hpoint, 0},
        {:timer_sel, @low_speed_timer}
      ]
    ]

    Enum.each(ledc_channel, fn channel_config -> :ok = LEDC.channel_config(channel_config) end)
    :ok = LEDC.fade_func_install(0)
    loop(ledc_channel)
  end

  def loop(ledc_channel) do
    :io.format('1. LEDC fade up to duty = ~p~n', [@test_duty])
    Enum.each(ledc_channel, fn channel_config -> do_stage_1(channel_config) end)
    Process.sleep(@test_fade_time)

    :io.format('2. LEDC fade down to duty = 0~n')
    Enum.each(ledc_channel, fn channel_config -> do_stage_2(channel_config) end)
    Process.sleep(@test_fade_time)

    :io.format('3. LEDC set duty = ~p without fade~n', [@test_duty])
    Enum.each(ledc_channel, fn channel_config -> do_stage_3(channel_config) end)
    Process.sleep(@test_fade_time)

    :io.format('4. LEDC set duty = 0 without fade~n')
    Enum.each(ledc_channel, fn channel_config -> do_stage_4(channel_config) end)
    Process.sleep(@test_fade_time)

    loop(ledc_channel)
  end

  defp do_stage_1(channel_config) do
    speed_mode = :proplists.get_value(:speed_mode, channel_config)
    channel = :proplists.get_value(:channel, channel_config)
    :ok = LEDC.set_fade_with_time(speed_mode, channel, @test_duty, @test_fade_time)
    :ok = LEDC.fade_start(speed_mode, channel, LEDC.fade_no_wait)
  end

  defp do_stage_2(channel_config) do
    speed_mode = :proplists.get_value(:speed_mode, channel_config)
    channel = :proplists.get_value(:channel, channel_config)
    :ok = LEDC.set_fade_with_time(speed_mode, channel, 0, @test_fade_time)
    :ok = LEDC.fade_start(speed_mode, channel, LEDC.fade_no_wait)
  end

  defp do_stage_3(channel_config) do
    speed_mode = :proplists.get_value(:speed_mode, channel_config)
    channel = :proplists.get_value(:channel, channel_config)
    :ok = LEDC.set_duty(speed_mode, channel, @test_duty)
    :ok = LEDC.update_duty(speed_mode, channel)
  end

  defp do_stage_4(channel_config) do
    speed_mode = :proplists.get_value(:speed_mode, channel_config)
    channel = :proplists.get_value(:channel, channel_config)
    :ok = LEDC.set_duty(speed_mode, channel, 0)
    :ok = LEDC.update_duty(speed_mode, channel)
  end
end
