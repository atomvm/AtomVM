#
# This file is part of AtomVM.
#
# Copyright 2018 Davide Bettio <davide@uninstall.it>
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

defmodule Blink do
  # suppress warnings when compileing the VM
  # not needed or recommended for user apps.
  @compile {:no_warn_undefined, [GPIO]}
  @pin 2

  def start() do
    GPIO.set_pin_mode(@pin, :output)
    loop(@pin, :low)
  end

  defp loop(pin, level) do
    GPIO.digital_write(pin, level)
    Process.sleep(200)
    loop(pin, toggle(level))
  end

  defp toggle(:high) do
    :low
  end

  defp toggle(:low) do
    :high
  end

end
