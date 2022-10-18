#
# This file is part of AtomVM.
#
# Copyright 2019-2020 Riccardo Binetti <rbino@gmx.com>
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

defmodule MultiBlink do
  # suppress warnings when compileing the VM
  # not needed or recommended for user apps.
  @compile {:no_warn_undefined, [GPIO]}
  def start do
    spawn(__MODULE__, :blink, [{:d, 12}, 1000])
    spawn(__MODULE__, :blink, [{:d, 13}, 500])
    spawn(__MODULE__, :blink, [{:d, 14}, 1500])
    spawn(__MODULE__, :blink, [{:d, 15}, 300])

    loop()
  end

  def loop do
    loop()
  end

  def blink(gpio, interval_ms) do
    gpio_driver = GPIO.open()
    GPIO.set_direction(gpio_driver, gpio, :output)

    blink_loop(gpio_driver, gpio, interval_ms, 0)
  end

  def blink_loop(gpio_driver, gpio, interval_ms, level) do
    GPIO.set_level(gpio_driver, gpio, level)

    :timer.sleep(interval_ms)

    blink_loop(gpio_driver, gpio, interval_ms, 1 - level)
  end
end
