#
# This file is part of AtomVM.
#
# Copyright 2026 Davide Bettio <davide@uninstall.it>
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

defmodule Esp.ADC do
  @moduledoc """
  Analog to digital peripheral support.

  Use this module to take ADC (analog voltage) readings. This driver currently
  supports the ESP32 family of chips only. The pins that can be used for ADC
  readings vary by device, check your datasheet for the specific hardware
  support.

  `Esp.ADC.init/0` and `Esp.ADC.deinit/1` acquire and release the ADC unit
  resource needed by all other functions. A channel resource used to take
  measurements from a pin is obtained with `Esp.ADC.acquire/4` and released with
  `Esp.ADC.release_channel/1`. Measurements are taken with `Esp.ADC.sample/3`.

  The Erlang `esp_adc` module offers the same functions plus convenience
  functions with default arguments and a gen_server managed API using pin
  numbers. Only one of the two sets of APIs may be used by an application.
  """

  @typedoc """
  Handle to an ADC unit or channel resource.
  """
  @type adc_rsrc() :: {:"$adc", reference(), reference()}

  @typedoc """
  Pin to be used for ADC readings.

  ADC capable pins vary by chipset. Consult your datasheet.
  """
  @type adc_pin() :: non_neg_integer()

  @typedoc """
  Resolution in bits of a measurement.

  `:bit_max` selects the highest value supported by the chipset. Some models
  only support a single fixed bit width.
  """
  @type bit_width() :: :bit_9 | :bit_10 | :bit_11 | :bit_12 | :bit_13 | :bit_max

  @typedoc """
  Decibel gain determining the maximum safe voltage to be measured.

  The specific range of voltages supported by each setting varies by device,
  consult your datasheet. `:db_11` has been superseded by `:db_12` and will be
  deprecated in a future release, but it remains the default because it is the
  maximum tolerated voltage on all builds.
  """
  @type attenuation() :: :db_0 | :db_2_5 | :db_6 | :db_11 | :db_12

  @typedoc """
  Option altering how a measurement is taken.

  `:raw` and `:voltage` determine whether these values are included in the
  result or returned as `:undefined`. The value of the `:samples` key is the
  number of samples to be taken and averaged when returning a measurement,
  default is 64.
  """
  @type read_option() :: :raw | :voltage | {:samples, 1..100_000}

  @typedoc """
  List of options altering how a measurement is taken.
  """
  @type read_options() :: [read_option()]

  @typedoc """
  Raw measurement.

  The maximum analog value is determined by `t:bit_width/0`.
  """
  @type raw_value() :: 0..8191 | :undefined

  @typedoc """
  Measurement in millivolts.

  The maximum safe millivolt value that can be measured is determined by
  `t:attenuation/0`, and should never exceed the maximum input tolerance of the
  chip.
  """
  @type voltage_reading() :: 0..3300 | :undefined

  @typedoc """
  Result of a measurement, as a raw value and a voltage in millivolts.
  """
  @type reading() :: {raw_value(), voltage_reading()}

  @doc """
  Initialize the ADC unit hardware.

  The returned ADC unit handle resource must be supplied to all other ADC
  operations.
  """
  @spec init() :: {:ok, adc_rsrc()} | {:error, term()}
  def init(),
    do: throw(:nif_error)

  @doc """
  Release the ADC unit resource returned from `init/0`.

  Stop the ADC driver and free the unit resource. All active ADC channels should
  be released with `release_channel/1` before freeing the unit resource.
  """
  @spec deinit(adc_rsrc()) :: :ok | {:error, term()}
  def deinit(_unit_resource),
    do: throw(:nif_error)

  @doc """
  Initialize an ADC pin and return a channel handle resource.

  The bit width `:bit_max` may be used to automatically select the highest
  sample rate supported by the chipset. The attenuation adjusts the gain, and
  therefore the safe voltage measurement range; see `t:attenuation/0`.

  Use the returned channel resource in subsequent ADC operations on the same
  pin.
  """
  @spec acquire(adc_pin(), adc_rsrc(), bit_width(), attenuation()) ::
          {:ok, adc_rsrc()} | {:error, term()}
  def acquire(_pin, _unit_handle, _bit_width, _attenuation),
    do: throw(:nif_error)

  @doc """
  Deinitialize the specified ADC channel.

  When an error is returned it is safe to drop the channel resource handle: once
  no process holds a reference to it any remaining resources associated with the
  channel are released as part of the next garbage collection event.
  """
  @spec release_channel(adc_rsrc()) :: :ok | {:error, term()}
  def release_channel(_channel_resource),
    do: throw(:nif_error)

  @doc """
  Take a reading from an ADC channel.

  When the read options contain `:raw` the raw value is returned in the first
  element of the reading, otherwise that element is `:undefined`. When they
  contain `:voltage` the voltage in millivolts is returned in the second
  element, otherwise that element is `:undefined`. The number of samples to be
  taken and averaged over is given by `{:samples, samples}`.

  An `{:error, :timeout}` result on a channel of ADC unit 2 usually means WiFi
  is enabled: unit 2 readings may be blocked until there is less network
  traffic.
  """
  @spec sample(adc_rsrc(), adc_rsrc(), read_options()) ::
          {:ok, reading()} | {:error, term()}
  def sample(_channel_resource, _unit_resource, _read_options),
    do: throw(:nif_error)
end
