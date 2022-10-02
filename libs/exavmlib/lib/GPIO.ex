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

  @spec open() :: pid()
  def open() do
    case :erlang.whereis(:gpio) do
      :undefined ->
        AVMPort.open({:spawn, "gpio"}, [])

      pid ->
        pid
    end
  end

  @spec read(pid(), gpio_pin()) :: :low | :high
  def read(gpio, gpio_num) do
    AVMPort.call(gpio, {:read, gpio_num})
  end

  @spec set_direction(pid(), gpio_pin(), direction()) :: :ok | :error
  def set_direction(gpio, gpio_num, direction) do
    AVMPort.call(gpio, {:set_direction, gpio_num, direction})
  end

  @spec set_level(pid(), gpio_pin(), digital_state()) :: :ok | :error
  def set_level(gpio, gpio_num, level) do
    AVMPort.call(gpio, {:set_level, gpio_num, level})
  end

  @spec set_int(pid(), non_neg_integer(), atom()) :: :ok | :error
  def set_int(gpio, gpio_num, trigger) do
    AVMPort.call(gpio, {:set_int, gpio_num, trigger})
  end

  @spec remove_int(pid(), non_neg_integer()) :: :ok | :error
  def remove_int(gpio, gpio_num) do
    AVMPort.call(gpio, {:remove_int, gpio_num})
  end

  @spec attach_interrupt(non_neg_integer(), atom()) :: :ok | :error
  def attach_interrupt(gpio_num, mode),
    do: set_int(open(), gpio_num, mode)

  @spec detach_interrupt(non_neg_integer()) :: :ok | :error
  def detach_interrupt(gpio_num),
    do: remove_int(:erlang.whereis(:gpio), gpio_num)

  @spec set_pin_mode(non_neg_integer(), atom()) :: :ok | :error
  def set_pin_mode(_gpio_num, _mode),
    do: throw(:nif_error)

  @spec digital_write(non_neg_integer(), non_neg_integer() | atom()) :: :ok | :error
  def digital_write(_gpio_num, _level),
    do: throw(:nif_error)

  @spec digital_read(non_neg_integer()) :: :high | :low
  def digital_read(_gpio_num),
    do: throw(:nif_error)

  @spec set_pin_pull(non_neg_integer(), atom()) :: :ok | :error
  def set_pin_pull(_gpio_num, _pull),
    do: throw(:nif_error)

  @spec hold_en() :: :ok | :error
  def hold_en(),
    do: throw(:nif_error)

  @spec hold_dis() :: :ok | :error
  def hold_dis(),
    do: throw(:nif_error)

  @spec deep_sleep_hold_en() :: :ok
  def deep_sleep_hold_en(),
    do: throw(:nif_error)

  @spec deep_sleep_hold_dis() :: :ok
  def deep_sleep_hold_dis(),
    do: throw(:nif_error)
end
