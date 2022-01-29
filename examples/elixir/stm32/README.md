<!--
 Copyright 2018-2019 Riccardo Binetti <rbino@gmx.com>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# STM32F4Discovery example

This example will blink the 4 LEDs present on the STM32F4Discovery with
different frequencies, using one process per LED.

# Building
From the root of the AtomVM repo, build the whole repo with
- `mkdir build && cd build`
- `cmake ..`
- `make -j4`

# Flashing
- Follow the STM32 README (`src/platforms/stm32`) to build and flash AtomVM
- From the root of the repo, `cd build/examples/elixir/stm32`
- Flash the packed AVM with `st-flash --reset write MultiBlink.avm 0x8080000`
