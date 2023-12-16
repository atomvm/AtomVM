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

defmodule I2C do
  @compile {:no_warn_undefined, [AVMPort]}
  @moduledoc """
  Functions for interacting with the i2c buss.
  """

  @typedoc """
  Valid GPIO pin number.

  The actual number of pins that are broken out vary by board and module.
  """
  @type gpio_pin() :: 0..48

  @typedoc """
  Clock speed of the i2c buss.
  """
  @type freq_hz() :: non_neg_integer()

  @typedoc """
  Valid parameters.

  Used to set the SCL pin, SDA pin, and clock speed.
  """
  @type param() ::
          {:scl, gpio_pin()}
          | {:sda, gpio_pin()}
          | {:clock_speed_hz, freq_hz()}

  @type params() :: [param()]

  @typedoc """
  I2C device address.

  Devices are addressed in the range of 0-127.
  """
  @type address() :: 0..127

  @typedoc """
  The register address in the device from which to read data
  """
  @type register() :: non_neg_integer()

  @doc """
  Open a connection to the I2C driver.

  Start the driver with a list of initialization parameters,
  see type param(). Returns the process id of the driver.

  ## Example:

  `I2C.open([{:scl, 15}, {:sda, 4}, {:clock_speed_hz, 1000000}])`
  """
  @spec open(params()) :: pid()
  def open(configuration) do
    AVMPort.open({:spawn, "i2c"}, configuration)
  end

  @doc """
  Begin a transmission of I2C commands.

  ## Parameters
    - driver:   pid returned by I2C.open()
    - device:   device address to communicate with

  This command is typically followed by one or more calls to
  write_byte/2 and then a call to end_transmission/1.
  """
  @spec begin_transmission(pid(), address()) :: :ok | {:error, term()}
  def begin_transmission(driver, device) do
    AVMPort.call(driver, {:begin_transmission, device})
  end

  @doc """
  Write a byte to the device.

  ## Parameters
    - driver:   pid returned by I2C.open()
    - data:     byte to be written

  This command must be wrapped in a begin_transmission/2
  and end_transmission/1 call.
  """
  @spec write_byte(pid(), byte()) :: :ok | {:error, term()}
  def write_byte(driver, data) do
    AVMPort.call(driver, {:write_byte, data})
  end

  @doc """
  Write a sequence of bytes to the device.

  ## Parameters
    - driver:   pid returned by I2C.open()
    - data:     binary data to be written

  This command must be wrapped in a begin_transmission/2
  and end_transmission/1 call.
  """
  @spec write_bytes(pid(), binary()) :: :ok | {:error, term()}
  def write_bytes(driver, data) do
    AVMPort.call(driver, {:write_bytes, data})
  end

  @doc """
  End a transmission of I2C commands

  ## Parameters
    - driver:   pid returned by I2C.open()

  This command is typically preceded by a call to begin_transmission/2
  and one or more calls to write_byte/2 or write_bytes/2.
  """
  @spec end_transmission(pid()) :: :ok | {:error, term()}
  def end_transmission(driver) do
    AVMPort.call(driver, {:end_transmission})
  end

  @doc """
  Read a block of bytes from the I2C device.

  ## Parameters
    - driver:   pid returned by I2C.open()
    - device:   device address to communicate with
    - count:    number of byte to read

  This command is not wrapped in a begin_transmission/2
  and end_transmission/1 call.
  """
  @spec read_bytes(pid(), address(), non_neg_integer()) :: {:ok, binary()} | {:error, term()}
  def read_bytes(driver, addr, count) do
    AVMPort.call(driver, {:read_bytes, addr, count})
  end

  @doc """
  Read a block of bytes from the I2C device starting at a specified
  register address.

  ## Parameters
    - driver:   pid returned by I2C.open()
    - device:   device address to communicate with
    - reg:      register address of the device to read from
    - count:    number of byte to read

  This command is not wrapped in a begin_transmission/2
  and end_transmission/1 call.
  """
  @spec read_bytes(pid(), address(), register(), non_neg_integer()) ::
          {:ok, binary()} | {:error, term()}
  def read_bytes(device, addr, reg, count) do
    AVMPort.call(device, {:read_bytes, addr, count, reg})
  end

  @doc """
  Write a block of bytes to the I2C device.

  ## Parameters
    - driver:   pid returned by I2C.open()
    - device:   device address to communicate with
    - data:     binary or byte value to be written

  This command is not wrapped in a begin_transmission/2
  and end_transmission/1 call.
  """
  @spec write_bytes(pid(), address(), binary() | byte()) :: :ok | {:error, term()}
  def write_bytes(driver, addr, data) do
    AVMPort.call(driver, {:write_bytes, addr, data})
  end

  @doc """
  Write a block of bytes to the I2C device starting at a specified
  register address.

  ## Parameters
    - driver:   pid returned by I2C.open()
    - device:   device address to communicate with
    - reg:      register address of the device to write to
    - data:     binary or byte value to be written

  This command is not wrapped in a begin_transmission/2
  and end_transmission/1 call.
  """
  @spec write_bytes(pid(), address(), register(), binary() | integer()) :: :ok | {:error, term()}
  def write_bytes(driver, addr, reg, data) do
    AVMPort.call(driver, {:write_bytes, addr, data, reg})
  end

  @doc """
  Close the connection to the I2C driver

  ## Parameters
    - driver:   pid returned by I2C.open()

  This function will close the connection to the I2C driver and
  free any resources in use by it.
  """
  @spec close(pid()) :: :ok | {:error, term()}
  def close(driver) do
    AVMPort.call(driver, {:close})
  end
end
