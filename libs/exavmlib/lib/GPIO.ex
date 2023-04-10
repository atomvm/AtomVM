#
# This file is part of AtomVM.
#
# Copyright 2018-2020 Davide Bettio <davide@uninstall.it>
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

defmodule GPIO do
  @compile {:no_warn_undefined, [AVMPort]}
  @moduledoc """
  Functions for interacting with GPIO pins of micro-controllers.
  """

  @typedoc """
  Valid GPIO pin number.

  The actual number of pins that are broken out vary by board and module.
  For espressif chips this a the pin number marked on the board or data sheet.
  For stm32 chips this is a tuple {GPIO_GROUP, PIN_NUMBER}.
  """
  @type gpio_pin() :: non_neg_integer() | {atom(), non_neg_integer()}

  @typedoc """
  Event type describing the voltage that will trigger an interrupt.

  :none is equivalent to removing the interrupt, no event will trigger.
  :both is rising or falling.
  """
  @type trigger() ::
          :none
          | :rising
          | :falling
          | :both
          | :low
          | :high

  @typedoc """
  The mode or direction used for a gpio pin.
  :output_od is output with open drain.
  """
  @type direction() ::
          :input
          | :output
          | :output_od

  @typedoc """
  Digital state of a gpio pin.

  Describes if a pin is high or low.
  """
  @type digital_state() ::
          :low
          | 0
          | :high
          | 1

  @typedoc """
  Internal resistor pull mode.

  Internal resistors can be set to pull a pin high, low, leave it floating
  or use both pull-up and pull down resistors to adjust the normal state of
  a pin for both input and output mode.
  :up_down is not useful for digital read or write functions, and will only
  be necessary in rare circumstances.
  """
  @type pull() ::
          :up
          | :down
          | :up_down
          | :floating

  @doc """
  Start the GPIO driver port

  GPIO.open() takes no arguments and returns a pid(). The pid will be
  registered as :gpio and can be found with Process:whereis(:gpio).

  The use of the GPIO port driver is only strictly necessary for the
  use of gpio interrupts, read and write functions have a nif alternative
  that should be slightly more efficient in most use cases.
  """
  @spec open() :: pid()
  def open() do
    case :erlang.whereis(:gpio) do
      :undefined ->
        AVMPort.open({:spawn, "gpio"}, [])

      pid ->
        pid
    end
  end

  @doc """
  Stop the gpio driver and remove any interrupts that have been set.
  Use the pid returned from GPIO.start/0 as a parameter.
  """
  @spec close(pid()) :: :ok | :error
  def close(gpio),
    do: AVMPort.call(gpio, {:close})

  @doc """
  Stop the gpio driver and remove any interrupts that have been set.
  Takes no parameters.
  """
  @spec stop() :: :ok | :error
  def stop(),
    do: close(:erlang.whereis(:gpio))

  @doc """
  Read the state of a gpio input pin.

  ## Parameters
    - gpio:      pid returned by GPIO.open()
    - gpio_num:  number of the pin to read

  Read if the state of an input pin is low (0) or high (1).
  GPIO.set_direction(pin, :input) must be used before reading a pin.

  This function has a nif equivalent digital_read/1
  """
  @spec read(pid(), gpio_pin()) :: :low | :high
  def read(gpio, gpio_num) do
    AVMPort.call(gpio, {:read, gpio_num})
  end

  @doc """
  Set the operational mode of a pin.

  ## Parameters
    - gpio:      pid returned by GPIO.open()
    - gpio_num:  number of the pin to configure
    - direction: mode :input, :output, or :output_od

  GPIO pins can be used for input, output, or output with open drain.

  This function has a nif equivalent set_pin_mode/2
  """
  @spec set_direction(pid(), gpio_pin(), direction()) :: :ok | :error
  def set_direction(gpio, gpio_num, direction) do
    AVMPort.call(gpio, {:set_direction, gpio_num, direction})
  end

  @doc """
  Set GPIO digital output level of a pin.

  ## Parameters
    - gpio:      pid returned by GPIO.open()
    - gpio_num:  number of the pin to configure
    - level:     digital output level to set

  Set a GPIO pin to :high (1) or :low (0).

  This function has a nif equivalent digital_write/2
  """
  @spec set_level(pid(), gpio_pin(), digital_state()) :: :ok | :error
  def set_level(gpio, gpio_num, level) do
    AVMPort.call(gpio, {:set_level, gpio_num, level})
  end

  @doc """
  Set an interrupt trigger on a gpio pin.

  ## Parameters
    - gpio:      pid returned by GPIO.open()
    - gpio_num:  number of the pin to set interrupt on
    - trigger:   voltage state to trigger the interrupt

    ### Triggers
      - :none     same as disabling an interrupt
      - :rising   increase in voltage level
      - :falling  decrease in voltage level
      - :both     rising or falling voltage level
      - :low      0 volts on pin
      - :high     high level (3.3V on ESP32)

  When the interrupt is triggered it will send a tuple:
    {:gpio_interrupt, pin}
  to the process that set the interrupt. 'pin' will be the pin
  number that was triggered, so multiple interrupts can be
  differentiated.
  """
  @spec set_int(pid(), gpio_pin(), trigger()) :: :ok | :error
  def set_int(gpio, gpio_num, trigger) do
    AVMPort.call(gpio, {:set_int, gpio_num, trigger})
  end

  @doc """
  Remove a gpio interrupt.

  ## Parameters
    - gpio:      pid returned by GPIO.open()
    - gpio_num:  number of the pin to remove the interrupt

  Removes an interrupt from the specified pin.
  """
  @spec remove_int(pid(), gpio_pin()) :: :ok | :error
  def remove_int(gpio, gpio_num) do
    AVMPort.call(gpio, {:remove_int, gpio_num})
  end

  @doc """
  This is a convenience function for set_int/3 that allows an interrupt
  to be set using only the pin number and trigger as arguments.

  ## Parameters
    - gpio_num:  number of the pin to set interrupt on
    - trigger:   voltage state to trigger the interrupt

    ### Triggers
      - :none     same as disabling an interrupt
      - :rising   increase in voltage level
      - :falling  decrease in voltage level
      - :both     rising or falling voltage level
      - :low      0 volts on pin
      - :high     high level (3.3V on ESP32)

  This function should only be used when only one gpio trigger is used
  in an application. If multiple pins are being configured with interrupt
  triggers set_int/3 should be used otherwise there is a race condition
  when open() is called internally by this function.
  """
  @spec attach_interrupt(gpio_pin(), trigger()) :: :ok | :error
  def attach_interrupt(gpio_num, trigger),
    do: set_int(open(), gpio_num, trigger)

  @doc """
  This is a convenience function for remove_int/2 that allows an interrupt
  to be removed using only the pin number as an argument.

  ## Parameters
    - gpio_num:  number of the pin to remove the interrupt

  Unlike attach_interrupt/2 this function can be safely used regardless of
  the number of interrupt pins used in the application.
  """
  @spec detach_interrupt(gpio_pin()) :: :ok | :error
  def detach_interrupt(gpio_num),
    do: remove_int(:erlang.whereis(:gpio), gpio_num)

  @doc """
  Initialize a pin to be used as a GPIO.

  ## Parameters
    - pin:  number of the pin to initialize

  Only required on RP2040 (Pico).
  """
  @spec init(gpio_pin()) :: :ok
  def init(_pin),
    do: :ok

  @doc """
  Deinitialize a pin so it's no longer used as a GPIO.

  ## Parameters
    - pin:  number of the pin to initialize

  Only implemented on RP2040 (Pico).
  """
  @spec deinit(gpio_pin()) :: :ok
  def deinit(_pin),
    do: :ok

  @doc """
  Set the directional mode of a gpio pin.

  ## Parameters
    - gpio_num:  number of the pin to configure
    - direction: mode :input, :output, or :output_od

  Used to set the direction of a pin before read or write operations.
  See '@type direction()' for more details.
  """
  @spec set_pin_mode(gpio_pin(), direction()) :: :ok | :error
  def set_pin_mode(_gpio_num, _mode),
    do: throw(:nif_error)

  @doc """
  Set the state of a gpio output pin.

    ## Parameters
    - gpio_num:  number of the pin to configure
    - level:     digital output level to set

  Set a gpio_pin() to the specified digital_state().
  GPIO.set_pin_mode(pin, :output | :output_od) must be used before writing to a pin.
  """
  @spec digital_write(gpio_pin(), digital_state()) :: :ok | :error
  def digital_write(_gpio_num, _level),
    do: throw(:nif_error)

  @doc """
  Read a gpio input pin.

  ## Parameters
    - gpio_num:  number of the pin to read

  Read if the state of an input pin is :low or :high.
  GPIO.set_pin_mode(pin, :input) must be used before reading from a pin.
  """
  @spec digital_read(gpio_pin()) :: :low | :high
  def digital_read(_gpio_num),
    do: throw(:nif_error)

  @doc """
  Set the internal resistor of a pin.

  ## Parameters
    - gpio_num:  number of the pin to configure
    - pull:      direction of internal resistor pull

    ### Pull Directions
     - :up        internal resistor pulled high
     - :down      internal resistor pulled to ground
     - :up_down   internal resistors in both directions
     - :floating  leave the pin floating

  """
  @spec set_pin_pull(gpio_pin(), pull()) :: :ok | :error
  def set_pin_pull(_gpio_num, _pull),
    do: throw(:nif_error)

  @doc """
  Lock the current state of a gpio pin.

  ## Parameters
    - gpio_num:  number of the pin to read

  This will prevent the state of an output pin from changing.
  Input pin values will not read any difference regardless of any change to the
  actual input to the pin.
  This hold only works during normal operation, to hold a pin during deep sleep
  see deep_sleep_hold_en/1.
  """
  @spec hold_en(gpio_pin()) :: :ok | :error
  def hold_en(_gpio_num),
    do: throw(:nif_error)

  @doc """
  Remove the lock on a gpio pin.

  ## Parameters
    - gpio_num:  number of the pin to read

  This will free a previously held pin by hold_en/1.
  """
  @spec hold_dis(gpio_pin()) :: :ok | :error
  def hold_dis(_gpio_num),
    do: throw(:nif_error)

  @doc """
  Lock the current state of all gpio pins during deep sleep only.

  This will hold the current state of all pins when the mcu goes into
  deep sleep and will release the hold when the device wakes up.

  Only implemented on esp32.
  """
  @spec deep_sleep_hold_en() :: :ok
  def deep_sleep_hold_en(),
    do: throw(:nif_error)

  @doc """
  Disable all digital gpio pad hold function during deep sleep.

  Only implemented on esp32.
  """
  @spec deep_sleep_hold_dis() :: :ok
  def deep_sleep_hold_dis(),
    do: throw(:nif_error)
end
