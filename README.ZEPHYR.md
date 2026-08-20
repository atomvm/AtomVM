<!--
 Copyright 2023 Winford (Uncle Grumpy) <winford@object.stream>
 Copyright 2026 Peter M <petermm@gmail.com>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM for Zephyr

This is an experimental port that will hopefully replace the current STM32 `libopencm3` based port, and should support many more boards.

Not all boards supported by Zephyr are capable of running AtomVM. The most likely excluding factor will be lack of flash storage space to accommodate both the VM and user BEAM applications. Currently a build can be attempted for any board supported by the Zephyr SDK, and the user will need to determine if the final build can fit onto the device and still have space left for a user application partition (on STM32 devices this is typically a 128K block size).

This port is currently configured to target **Zephyr RTOS v4.4.2** and **Zephyr SDK v1.0.1**.

## Prerequisites (Local Build)

If you plan to build directly on your host machine:

* `west` - Make sure to follow its [installation procedure](https://docs.zephyrproject.org/latest/develop/getting_started/index.html#get-zephyr-and-install-python-dependencies)
* `Zephyr SDK` - [installation procedure](https://docs.zephyrproject.org/latest/develop/getting_started/index.html#install-the-zephyr-sdk) (v1.0.1 is recommended to match the RTOS version)
* `cmake`
* `ninja`
* An appropriate flashing tool and software for your device, such as [st-flash](https://github.com/texane/stlink) for flashing STM32 devices with a [st-link v2](https://www.st.com/en/development-tools/st-link-v2.html) or [st-link v3](https://www.st.com/en/development-tools/stlink-v3set.html) device.
* A serial console program, such as `minicom` or `screen`, so that you can view console output from your AtomVM application.

## Building (Local Build)

Before building for the first time you need to have set up `west` and `Zephyr SDK`, following the [Zephyr Project Getting Started instructions](https://docs.zephyrproject.org/latest/develop/getting_started/index.html). After setup is complete, from inside the `src/platforms/zephyr` directory, use `west` to build for your board:

    $ west build -b nucleo_f429zi -p=auto .
    $ west build -b esp32c3_devkitm/esp32c3 -p=auto .
    $ west build -b esp32s3_devkitc/esp32s3/procpu -p=auto .

The `-p=auto` option instructs `west` to perform a pristine build if changes have been made, which is a recommended practice.

## Building with Docker

If you do not want to install `west` and the Zephyr SDK on your host system, you can build the image inside a local Docker container (e.g. using Docker Desktop or OrbStack).

A helper script is provided at `src/platforms/zephyr/docker-build.sh`. To build:

    $ cd src/platforms/zephyr
    $ ./docker-build.sh -b nucleo_f429zi
    $ ./docker-build.sh -b esp32c3_devkitm/esp32c3
    $ ./docker-build.sh -b esp32s3_devkitc/esp32s3/procpu

This will:
1. Build a local Docker image named `atomvm-zephyr-build` based on `ghcr.io/zephyrproject-rtos/ci:v0.29.2` (which bundles Zephyr SDK v1.0.1).
2. Clone the Zephyr project dependencies inside the container's workspace using `west update`.
3. Build the AtomVM firmware for the specified board (`nucleo_f429zi` by default).

The build outputs will be generated in `src/platforms/zephyr/build/zephyr/AtomVM.elf` on your host.

## Listing `west` target devices

A complete list of boards supported by your Zephyr installation can be viewed by running:

    $ west boards

## Flashing

To flash the compiled binary onto your connected board:

    $ west flash

## Debugging

To start a live gdb session on the device use:

    $ west attach
