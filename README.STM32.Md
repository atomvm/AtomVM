<!--
 Copyright 2018 Riccardo Binetti <rbino@gmx.com>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM for STM32

## Prerequisites

* [st-flash](https://github.com/texane/stlink), to flash both AtomVM and your packed AVM applications. Make sure to follow its [installation procedure](https://github.com/texane/stlink#installation) before proceeding further.
* arm-none-eabi/arm-elf toolchain compatible with your system
* `cmake`
* `make`
* A serial console program, such as `minicom` or `screen`, so that you can view console output from your AtomVM application.

## Building
Before building for the first time you need to have a compiled clone of the libopencm3 libraries, from inside the AtomVM/src/platforms/stm32 directory:

    $ git clone https://github.com/libopencm3/libopencm3.git
    $ cd libopencm3 && make -j4 && cd ..
    $ mkdir build
    $ cd build
    $ cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/arm-toolchain.cmake
    $ make

You can put libopencm3 wherever you want on your PC as long as you update LIBOPENCM3_DIR to point to it. This example assumes it has been cloned into /opt/libopencm3 and built. From inside the AtomVM/src/platforms/stm32 directory:

	$ mkdir build
	$ cd build
    $ cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/arm-toolchain.cmake -DLIBOPENCM3_DIR=/opt/libopencm3 ..
    $ make

## Flashing
To flash AtomVM, use

    $ st-flash --reset write AtomVM-stm32f407vgt6.bin 0x8000000

To flash your packed AVM, use

    $ st-flash --reset write /path/to/your/packed.avm 0x8080000

You must include the atomvmlib.avm with your application when using packbeam, and it should be pruned:

	$ packbeam create -p -i application.avm application.beam /path/to/AtomVM/build/libs/atomvmlib/atomvmlib.avm

> Note: The option`-i` will instruct packbeam to include file names and line numbers in stack traces. This makes debugging applications far easier, but also increases size, so it may be omitted if desired. The `-p` option should be used, it instructs packbeam to prune the unused functions from the packed `.avm` file, and is strongly recommended.

AtomVM expects to find the AVM at the address 0x808000. On a STM32 Discovery board this means that the 1MB of flash will be split in 512KB available for the program and 512KB available for the packed AVM. If for any reason you want to modify this, you can change `AVM_ADDRESS` and `AVM_FLASH_MAX_SIZE` defines in `main.c`.

## Changing device

The default build is based on the STM32F4Discovery board chip (`stm32f407vgt6`). If you want to target a different
chip, pass the `-DDEVICE` flag when invoking cmake. For example, to use the STM32F429Discovery, pass
`-DDEVICE=stm32f429zit6`

If you are building for a different target board the `CLOCK_FREQUENCY` definition in main.c will need to be changed to match the clock frequency (in hertz) of your cpu.

The rcc_clock_setup_XXX_XXX will also need to be changed to match your particular chip-set. Consult [ST's documentation](https://www.st.com/en/microcontrollers-microprocessors/stm32-32-bit-arm-cortex-mcus.html) for appropriate settings.

## Printing
By default, stdout and stderr are printed on USART2. On the STM32F4Discovery board, you can see them
using a TTL-USB with the TX pin connected to board's pin PA2 (USART2 RX). Baudrate is 115200 and serial transmission
is 8N1 with no flow control.

> If building for a different target USART and gpio pins may need to be adjusted in `main.c`.
