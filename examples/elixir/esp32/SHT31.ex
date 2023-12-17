#
#  This file is part of AtomVM.
#
#  Copyright 2019-2020 Davide Bettio <davide@uninstall.it>
#  Copyright 2022 Winford (Uncle Grumpy) <dwinford@proton.me>
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#
#  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

defmodule SHT31 do
  # suppress warnings when compiling the VM
  # not needed or recommended for user apps.
  @compile {:no_warn_undefined, [I2C]}

  import Bitwise

  @sht31_base_addr 0x44
  @sht31_meas_high_rep 0x2400

  def start do
    i2c = I2C.open([{:scl, 15}, {:sda, 4}, {:clock_speed_hz, 1000000}])
    loop(i2c)
  end

  defp loop(i2c) do
    val = read(i2c)
    :erlang.display(val)
    Process.sleep(10000)
    loop(i2c)
  end

  defp read(i2c) do
    {:ok, bin} = read_sensor(i2c)
    parse_bin(bin)
  end

  defp parse_bin(bin) do
    int_temp = (:binary.at(bin, 0) <<< 8) ||| :binary.at(bin, 1)
    temp = float_temp(int_temp, 0.01)
    int_hum = (:binary.at(bin, 3) <<< 8) ||| :binary.at(bin, 4)
    hum = float_hum(int_hum, 0.01)
    {:ok, temp, hum}
  end

  defp float_temp(int_temp, s) do
    (((4375 * int_temp) >>> 14) - 4500) * s
  end

  defp float_hum(int_hum, s) do
    ((625 * int_hum) >>> 12) * s
  end

  defp read_sensor(i2c) do
    send_command(i2c, @sht31_meas_high_rep)
    Process.sleep(20)
    I2C.read_bytes(i2c, @sht31_base_addr, 6)
  end

  defp send_command(i2c, command) do
    I2C.begin_transmission(i2c, @sht31_base_addr)
    I2C.write_byte(i2c, (command >>> 8) &&& 0xFF)
    I2C.write_byte(i2c, command &&& 0xFF)
    I2C.end_transmission(i2c)
  end
end
