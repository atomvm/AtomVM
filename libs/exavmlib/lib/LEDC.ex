#
# This file is part of AtomVM.
#
# Copyright 2018-2022 Davide Bettio <davide@uninstall.it>
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

defmodule LEDC do
  @moduledoc """
  LED Controller low-level APIs.

  The functions in this module broadly reflect the ESP IDF-SDK LED Controller API. See the IDF-SDK
  <a href="https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/ledc.html">LEDC</a>
  documentation for more information about these APIs.
  """

  @typedoc """
  Valid ledc timer.

  LEDC provides 4 timers numbered 0 through 3.
  """
  @type ledc_timer() :: 0..3

  @typedoc """
  Valid timer number configuration
  """
  @type timer_num_cfg() :: {:timer_num, ledc_timer()}

  @typedoc """
  Valid ledc channel.

  LEDC provides 8 channels numbered 0 through 7.
  """
  @type ledc_channel() :: 0..7

  @typedoc """
  Valid channel configuration.
  """
  @type channel_cfg() :: {:channel, ledc_channel()}

  @typedoc """
  Duty resolution (in bits)

  The LEDC timer bit width can be set from 1 to 20 bits.
  """
  @type duty_resolution() :: 1..20

  @typedoc """
  Valid duty resolution configuration.
  """
  @type duty_resolution_cfg() :: {:duty_resolution, duty_resolution()}

  @typedoc """
  Frequency of LEDC in Hz

  See: https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/ledc.html#supported-range-of-frequency-and-duty-resolutions
  for information on the effects of this frequency on duty resolution.
  """
  @type freq_hz() :: 1..40_000_000

  @typedoc """
  Valid driver frequency configuration.
  """
  @type freq_hz_cfg() :: {:freq_hz, freq_hz()}

  @typedoc """
  Valid speed mode values are integers 0 (high speed) or 1 (low speed)
  or use the convenience functions LEDC.high_speed_mode/0 or LEDC.low_speed_mode/0.
  """
  @type speed_mode() :: 0 | 1
  @typedoc """
  Valid speed mode configuration.
  """
  @type speed_mode_cfg() :: {:speed_mode, speed_mode()}

  @typedoc """
  Complete configuration for a LEDC timer.
  """
  @type timer_config() :: [
          duty_resolution_cfg() | freq_hz_cfg() | speed_mode_cfg() | timer_num_cfg()
        ]

  @typedoc """
  The LEDC duty setting range is [0, (2**duty_resolution) - 1]
  max duty_resolution = 20, results in 2**20 - 1 = 1048575
  """
  @type duty() :: 0..1_048_575

  @typedoc """
  Valid duty configuration.
  """
  @type duty_cfg() :: {:duty, duty()}

  @typedoc """
  Valid GPIO pin number.

  The actual number of pins that are broken out vary by board and module.
  """
  @type gpio_pin() :: 0..48

  @typedoc """
  Valid gpio pin configuration
  """
  @type gpio_num_cfg() :: {:gpio_num, gpio_pin()}

  @typedoc """
  The LEDC hpoint value(max: 0xfffff)
  """
  @type hpoint() :: 0..1_048_575

  @typedoc """
  Valid hpoint configuration
  """
  @type hpoint_cfg() :: {:hpoint, hpoint()}

  @typedoc """
  Valid timer select configuration.
  """
  @type timer_sel_cfg() :: {:timer_sel, ledc_timer()}

  @typedoc """
  Complete Channel configuration.
  """
  @type channel_config() :: [
          channel_cfg()
          | duty_cfg()
          | gpio_num_cfg()
          | speed_mode_cfg()
          | hpoint_cfg()
          | timer_sel_cfg()
        ]

  @typedoc """
  Error codes.

  -1 indicates a failure.
  0 is returned on success.
  positive integers represent specific errors.
  See Espressif's esp-idf/components/esp_common/include/esp_err.h for details.
  """
  @type ledc_error_code() :: integer()

  @typedoc """
  Set blocking or non-blocking mode.

  Valid values are 0 (non-blocking) or 1 (blocking),
  Optionally use the convenience functions LEDC.fade_no_wait/0
  (non-blocking) or LEDC.fade_wait_done/0 (blocking).
  """
  @type fade_mode() :: 0..1

  @doc """
  LEDC channel configuration.

  Configure LEDC channel with a list containing
  [channel_cfg | duty_cfg | gpio_num_cfg | speed_mode_cfg | hpoint_cfg | timer_sel_cfg]
  """
  @spec channel_config(channel_config()) :: :ok | {:error, ledc_error_code()}
  def channel_config(_config),
    do: throw(:nif_error)

  @doc """
  LEDC timer configuration.

  Configure LEDC timer with a list containing
  [source_timer | speed_mode | frequency(Hz) |duty_resolution]
  """
  @spec timer_config(timer_config()) :: :ok | {:error, ledc_error_code()}
  def timer_config(_config),
    do: throw(:nif_error)

  @doc """
  Install LEDC fade function.

  Flags used to allocate the interrupt. One or multiple (ORred)
  ESP_INTR_FLAG_* values. See esp_intr_alloc.h for more info.

  This function will occupy interrupt of LEDC module.
  """
  @spec fade_func_install(non_neg_integer()) :: :ok | {:error, ledc_error_code()}
  def fade_func_install(_flags),
    do: throw(:nif_error)

  @doc """
  Uninstall LEDC fade function.
  """
  @spec fade_func_uninstall() :: :ok
  def fade_func_uninstall(),
    do: throw(:nif_error)

  @doc """
  Set LEDC fade function, with a limited time.

  `speed` configures the LEDC channel group with specified speed mode.
  Use the function LEDC.high_speed_mode/0 | LEDC.low_speed_mode/0
  or integers 0|1. Caution, not all targets support high speed mode.
  `channel` is of type ledc_channel(), (0-7).
  `target_duty` is duty of fading.(0..(2^duty_resolution-1))).
  `max_fade_ms` sets the maximum time of the fading (ms).

  Note. Call LEDC.fade_func_install() once before calling this function.
  Call LEDC.fade_start() after this to start fading.
  """
  @spec set_fade_with_time(
          speed_mode(),
          ledc_channel(),
          duty(),
          non_neg_integer()
        ) :: :ok | {:error, ledc_error_code()}
  def set_fade_with_time(_speed, _channel, _target_duty, _max_fade_ms),
    do: throw(:nif_error)

  @doc """
  Set LEDC fade function.

  `speed` configures the LEDC channel group with specified speed mode.
  Use the function LEDC.high_speed_mode/0 | LEDC.low_speed_mode/0
  or integers 0|1. Caution, not all targets support high speed mode.
  `channel` is of type ledc_channel(), (0-7).
  `target_duty` is duty of fading.(0..(2^duty_resolution-1))).
  `scale` controls the increase or decrease step scale.
  `cycle_num` is the number of cycles increase or decrease the duty

  Note. Call LEDC.fade_func_install() once before calling this function.
  Call LEDC.fade_start() after this to start fading.
  """
  @spec set_fade_with_step(
          speed_mode(),
          ledc_channel(),
          duty(),
          non_neg_integer(),
          non_neg_integer()
        ) :: :ok | {:error, ledc_error_code()}
  def set_fade_with_step(_speed, _channel, _target_duty, _scale, _cycle_num),
    do: throw(:nif_error)

  @doc """
  Start LEDC fading.

  `speed` configures the LEDC channel group with specified speed mode.
  Use the function LEDC.high_speed_mode/0 | LEDC.low_speed_mode/0
  or integers 0|1. Caution, not all targets support high speed mode.
  `channel` is of type ledc_channel(), (0-7).
  `fade_mode` controls whether or not to block until fading done, use the
  convenience functions LEDC.fade_no_wait/0 | LEDC.fade_wait_done/0 or integers 0|1.

  Note. Call LEDC.fade_func_install() once before calling this function.
  Call LEDC.fade_start() after this to start fading.
  """
  @spec fade_start(speed_mode(), ledc_channel(), fade_mode()) :: :ok | {:error, ledc_error_code()}
  def fade_start(_speed, _channel, _mode),
    do: throw(:nif_error)

  @doc """
  LEDC get duty.

  `speed` configures the LEDC channel group with specified speed mode.
  Use the function LEDC.high_speed_mode/0 | LEDC.low_speed_mode/0
  or integers 0|1. Caution, not all targets support high speed mode.
  `channel` is of type ledc_channel(), (0-7).
  """
  @spec get_duty(speed_mode(), ledc_channel()) :: non_neg_integer() | {:error, ledc_error_code()}
  def get_duty(_speed, _channel),
    do: throw(:nif_error)

  @doc """
  LEDC set duty.

  `speed` configures the LEDC channel group with specified speed mode.
  Use the function LEDC.high_speed_mode/0 | LEDC.low_speed_mode/0
  or integers 0|1. Caution, not all targets support high speed mode.
  `channel` is of type ledc_channel(), (0-7).
  `duty` in the range of [0, (2**duty_resolution)].
  """
  @spec set_duty(speed_mode(), ledc_channel(), duty()) ::
          :ok | {:error, ledc_error_code()}
  def set_duty(_speed, _channel, _duty),
    do: throw(:nif_error)

  @doc """
  LEDC update channel parameters.

  `speed` configures the LEDC channel group with specified speed mode.
  Use the function LEDC.high_speed_mode/0 | LEDC.low_speed_mode/0
  or integers 0|1. Caution, not all targets support high speed mode.
  `channel` is of type ledc_channel(), (0-7).
  """
  @spec update_duty(speed_mode(), ledc_channel()) :: :ok | {:error, ledc_error_code()}
  def update_duty(_speed, _channel),
    do: throw(:nif_error)

  @doc """
  LEDC get channel frequency (Hz).

  `speed` configures the LEDC channel group with specified speed mode.
  Use the function LEDC.high_speed_mode/0 | LEDC.low_speed_mode/0
  or integers 0|1. Caution, not all targets support high speed mode.
  `timer_num` is the timer 0-3.
  """
  @spec get_freq(speed_mode(), ledc_timer()) :: non_neg_integer() | {:error, ledc_error_code()}
  def get_freq(_speed, _timer_num),
    do: throw(:nif_error)

  @doc """
  LEDC set channel frequency (Hz).

  `speed` configures the LEDC channel group with specified speed mode.
  Use the function LEDC.high_speed_mode/0 | LEDC.low_speed_mode/0
  or integers 0|1. Caution, not all targets support high speed mode.
  `timer_num` is the timer 0-3.
  `freq` is the desired channel frequency in Hz.
  """
  @spec set_freq(speed_mode(), ledc_timer(), freq_hz()) ::
          non_neg_integer() | {:error, ledc_error_code()}
  def set_freq(_speed, _timer_num, _freq),
    do: throw(:nif_error)

  @doc """
  LEDC stop. Disable LEDC output, and set idle level.

  `speed` configures the LEDC channel group with specified speed mode.
  Use the function LEDC.high_speed_mode/0 | LEDC.low_speed_mode/0
  or integers 0|1. Caution, not all targets support high speed mode.
  `channel` is of type ledc_channel(), (0-7).
  `idle_level` configures the final output level for the led after the
  driver has been stopped.
  """
  @spec stop(speed_mode(), ledc_channel(), non_neg_integer()) :: :ok | {:error, ledc_error_code()}
  def stop(_speed, _channel, _idle_level),
    do: throw(:nif_error)

  @doc """
  Convenience function for setting ledc speed mode, note that not all targets support high speed mode.
  Function returns low speed mode on boards that do not support high speed mode.
  """
  @spec high_speed_mode() :: 0
  def high_speed_mode(),
    do: 0

  @doc """
  Convenience function for setting ledc low speed mode.
  """
  @spec low_speed_mode() :: 0 | 1
  def low_speed_mode() do
    case :erlang.system_info(:esp32_chip_info) do
      %{model: :esp32} -> 1
      _ -> 0
    end
  end

  @doc """
  Convenience function for setting ledc fade mode
  """
  @spec fade_no_wait() :: 0
  def fade_no_wait(),
    do: 0

  @doc """
  Convenience function for setting ledc fade mode
  """
  @spec fade_wait_done() :: 1
  def fade_wait_done(),
    do: 1
end
