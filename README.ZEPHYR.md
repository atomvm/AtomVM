<!--
 Copyright 2023 Winford (Uncle Grumpy) <winford@object.stream>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM for Zephyr
This is an experimental port that will hopefully replace the current stm32 `libopencm3` based port, and should support many more boards.
Not all boards supported by Zephyr are capable of running AtomVM. The most likely excluding factor will be lack of flash storage space to accommodate both the VM and user BEAM applications. We will likely need a customized device tree to only define the boards that AtomVM can support. Currently a build can be attempted for any board supported by the Zephyr SDK and the user will need to determining if the final build can fit onto the device and still have space left for a user application partition (on stm32 devices this is typically a 128K block size).

## Prerequisites

* `west` Make sure to follow its [installation procedure](https://docs.zephyrproject.org/latest/develop/getting_started/index.html#get-zephyr-and-install-python-dependencies)
* `Zephyr SDK` [installation procedure](https://docs.zephyrproject.org/latest/develop/getting_started/index.html#install-the-zephyr-sdk)
* `cmake`
* `ninja`
* An appropriate flashing tool and software for your device, such as [st-flash](https://github.com/texane/stlink) for flashing STM32 devices with a [st-link v2](https://www.st.com/en/development-tools/st-link-v2.html) or [st-link v3](https://www.st.com/en/development-tools/stlink-v3set.html) device.
* A serial console program, such as `minicom` or `screen`, so that you can view console output from your AtomVM application.

## Building
Before building for the first time you need to have set up `west` and `Zephyr SDK`, following the [Zephyr Project Getting Started instructions](https://docs.zephyrproject.org/latest/develop/getting_started/index.html). After setup is complete from inside the AtomVM/src/platforms/zephyr directory use `west` to build for your board with the `-b` switch:

    $ west build -b nucleo_f429zi -p=auto .

The `-p=auto` option instructs `west` to perform a pristine build if changes have been made, which is a recommended practice.

## Listing `west` target devices
A complete list of boards supported by

    $ west boards

## Flashing

    $ west flash

## Debugging
To start a live gdb session on the device use:

    $ west attach
