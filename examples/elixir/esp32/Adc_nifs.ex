#
# This file is part of AtomVM.
#
# Copyright 2024 Winford <winford@object.stream>
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


defmodule ADCnifs do
  # suppress warnings when compiling the VM
  @compile {:no_warn_undefined, [Esp.ADC]}
  @pin 33

  def start() do
    IO.puts("Testing ADC on pin #{@pin}")
    {:ok, unit} = Esp.ADC.init()
    {:ok, chan} = Esp.ADC.acquire(@pin, unit, :bit_max, :db_12)
    loop(@pin, unit, chan)
  end

  defp loop(pin, unit, chan) do
    case Esp.ADC.sample(chan, unit, [:raw, :voltage, {:samples, 64}]) do
      {:ok, {raw, mv}} ->
        IO.puts("Pin #{pin} value = #{raw}, millivolts = #{mv}")
      error ->
        IO.puts("Error taking ADC sample from pin #{pin}: #{error}")
    end
    Process.sleep(500)
    loop(pin, unit, chan)
  end

end
