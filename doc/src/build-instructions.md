<!--
 Copyright 2021-2022 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Build Instructions

This guide is intended for anyone interested in building the AtomVM virtual machine from source code.  You may be interested in building the AtomVM source code if you want to provide bug fixes or enhancements to the VM, or if you want to simply learn more about the platform.  In addition, some "downstream" drivers for specific devices may need to be built specifically for the target platform (e.g., ESP32), in which case building the VM from source code is required.

```{tip}
Many applications do not require building the AtomVM runtime from source code.  Instead, you can
[download pre-built VM images](https://github.com/atomvm/AtomVM/releases) for platforms such as
ESP32, and use Erlang and Elixir tooling to build and deploy your applications.
```

The AtomVM virtual machine itself, including the runtime code execution engine, as well as built-in functions and Nifs is implemented in C.  The core standard and AtomVM libraries are implemented in Erlang and Elixir.

The native C parts of AtomVM compile to machine code on MacOS, Linux, and FreeBSD platforms.  The C code also compiles to run on the ESP32 and STM32 platforms.  Typically, binaries for these platforms are created on a UNIX-like environment (MacOS or Linux, currently) using tool-chains provided by device vendors to cross-compile and target specific device architectures.

The Erlang and Elixir parts are compiled to BEAM byte-code using the Erlang (`erlc`) and Elixir compilers.  For information about specific versions of required software, see the [Release Notes](./release-notes.md).

This guide provides information about how to build AtomVM for the various supported platforms (Generic UNIX, ESP32, and STM32).

```{attention}
In order to build AtomVM AVM files for ESP32 and STM32 platforms, you will also need to build
AtomVM for the Generic UNIX platform of your choice.
```

## Downloading AtomVM

The AtomVM source code is available by cloning the AtomVM github repository:

```shell
$ git clone https://github.com/atomvm/AtomVM
```

```{seealso}
Downloading the AtomVM github repository requires the installation of the `git` program.  Consult
your local OS documentation for installation of the `git` package.
```

If you want to build a release version of AtomVM, simply checkout the desired release:

```shell
$ git checkout v0.6.0-alpha.2
```

```{tip}
You may need to refresh the tags if you have already cloned the repository and you want to build a
more recent release version.

    $ git pull --tags --rebase

The use of `--rebase` is necessary if you are in a working branch and have made commits, otherwise
it is optional.
```

To return to the current `master` branch use `git switch master`.

## Source code organization

Source code is organized as follows:

* `src`  Contains the core AtomVM virtual machine source code;
* `lib`  Contains the Erlang and Elixir core library source code;
* `tools` Contains AtomVM tooling, including the `PackBEAM` executable, as well as build support tooling;
* `examples` Contains sample programs for demonstration purposes;
* `tests` Contains test code run as part of test qualification;
* `doc` Contains documentation source code and content.

The `src` directory is broken up into the core platform-independent AtomVM library (`libAtomVM`), and platform-dependent code for each of the supported platforms  (Generic UNIX, ESP32, and STM32).

## External dependencies

### `uf2tool`

AtomVM depends on `uf2tool`. It is used to pack both native and Erlang/Elixir/Gleam code for RP2. uf2tool is downloaded automatically by `rebar3` from `hex` mirrors.

It is possible to use a local copy of uf2tool by setting `UF2TOOL_PATH` variable to a path to a source checkout of [`uf2tool`](https://github.com/pguyot/uf2tool) when invoking CMake.

## Platform Specific Build Instructions

* [Generic UNIX](#building-for-generic-unix)
* [ESP32](#building-for-esp32)
* [STM32](#building-for-stm32)
* [RP2](#building-for-rp2) (including Pico boards)
* [WASM](#building-for-emscripten) (NodeJS or web)

## Building for Generic UNIX

The following instructions apply to unix-like environments, including Linux, FreeBSD, DragonFly and MacOS.

```{hint}
The Generic UNIX is useful for running and testing simple AtomVM programs.  Not all of the AtomVM
APIs, specifically, APIs that are dependent on various device integration, are supported on this
platform.
```

### Generic UNIX Build Requirements

The following software is required in order to build AtomVM in generic UNIX systems:

* `gcc` or `llvm` tool chains
* `cmake`
* `make`
* `gperf`
* `zlib`
* `Mbed TLS`
* Erlang/OTP compiler (`erlc`)
* Elixir compiler

Consult [Release Notes](./release-notes.md) for currently supported versions of required software.

Consult your local OS documentation for instructions about how to install these components.

### Generic UNIX Build Instructions

The AtomVM build for generic UNIX systems makes use of the `cmake` tool for generating `make` files from the top level AtomVM directory.  With CMake, you generally create a separate directory for all output files (make files, generated object files, linked binaries, etc).  A common pattern is to create a local `build` directory, and then point `cmake` to the parent directory for the root of the source tree:

```shell
$ mkdir build
$ cd build
$ cmake ..
```

This command will create all of the required make files for creating the AtomVM binary, tooling, and core libraries.  You can create all of these object using the `make` command:

```shell
$ make -j 8
```

```{tip}
You may specify `-j <n>`, where `<n>` is the number of CPUs you would like to assign to run the
build in parallel.
```

Upon completion, the `AtomVM` executable can be found in the `build/src` directory.

The AtomVM core Erlang library can be found in the generated `libs/atomvmlib.avm` AVM file.

Use the `install` target to install the `atomvm` command and associated binary files.  On most UNIX systems, these artifacts will be installed in the `/usr/local` directory tree.

```{attention}
On some systems, you may need to run this target with `root` or `sudo` permissions.

    $ sudo make install

```

Once installed, you can use the `atomvm` command to execute an AtomVM application.  E.g.,

```shell
$ atomvm /path/to/myapp.avm
```

For users doing incremental development on the AtomVM virtual machine, you may want to run the `AtomVM` binary directly instead of installing the VM on your machine.  If you do, you will typically need to also specify the path to the AtomVM core Erlang library.  For example,

```shell
$ cd build
$ ./src/AtomVM /path/to/myapp.avm ./libs/atomvmlib.avm
```

#### Special Note for MacOS users

You may build an Apple Xcode project, for developing, testing, and debugging in the Xcode IDE, by specifying the Xcode generator.  For example, from the top level AtomVM directory:

```shell
$ mkdir xcode
$ cmake -G Xcode ..
...
$ open AtomVM.xcodeproj
```

The above commands will build and open an AtomVM project in the Xcode IDE.

### Running tests

There are currently two sets of suites of tests for AtomVM:

* Erlang tests (`erlang_tests`) A set of unit tests for basic Erlang functionality, exercising support BEAM opcodes, built-in functions (Bifs) and native functions (Nifs).
* Library tests, exercising functionality in the core Erlang and Elixir libraries.

To run the Erlang tests, run the `test-erlang` executable in the `tests` directory:

```shell
$ ./tests/test-erlang
```

This will run a suite of several score unit tests.  Check the status of the executable after running the tests.  A non-zero return value indicates a test failure.

To run the Library tests, run the corresponding AVM module in the `tests/libs` directory using the `AtomVM` executable.  For example:

```shell
$ ./src/AtomVM ./tests/libs/estdlib/test_estdlib.avm
```

This will run a suite of several unit tests for the specified library.  Check the status of the executable after running the tests.  A non-zero return value indicates a test failure.

Tests for the following libraries are supported:

* `estdlib`
* `eavmlib`
* `alisp`

## Building for ESP32

Building AtomVM for ESP32 must be done on either a Linux or MacOS build machine.

In order to build a complete AtomVM image for ESP32, you will also need to build AtomVM for the Generic UNIX platform (typically, the same build machine you are suing to build AtomVM for ESP32).

### ESP32 Build Requirements

The following software is required in order to build AtomVM for the ESP32 platform:

* Espressif Xtensa tool chains
* [Espressif IDF SDK](https://www.espressif.com/en/products/sdks/esp-idf) (consult [Release Notes](./release-notes.md) for currently supported versions)
* `cmake`

Instructions for downloading and installing the Espressif IDF SDK and tool chains are outside of the scope of this document.  Please consult the [IDF SDKGetting Started](https://docs.espressif.com/projects/esp-idf/en/release-v4.4/get-started/index.html) guide for more information.

### ESP32 Build Instructions

To activate the ESP-IDF build environment change directories to the tree root of your local ESP-IDF:

```shell
$ cd <ESP-IDF-ROOT-DIR>
$ . ./export.sh
```

```{hint}
If you followed Espressif's installation guide the ESP-IDF directory is `${HOME}/esp/esp-idf`
```

Change directories to the `src/platforms/esp32` directory under the AtomVM source tree root:

```shell
$ cd <atomvm-source-tree-root>
$ cd src/platforms/esp32
```

If you want to build an image with Elixir modules included you must first have a version of Elixir installed that is compatible with your OTP version, then add the following line to sdkconfig.defaults:
```shell
CONFIG_PARTITION_TABLE_CUSTOM_FILENAME="partitions-elixir.csv"
```

Start by updating the default build configuration of local `sdkconfig` file via the `idf.py reconfigure` command:

```shell
$ idf.py set-target esp32
$ idf.py reconfigure
```

```{tip}
For those familiar with esp-idf the build can be customized using `menuconfig` instead of
`reconfigure`:

    $ idf.py menuconfig

This command will bring up a curses dialog box where you can make adjustments such as not including
AtomVM components that are not desired in a particular build. You can also change the behavior of a
crash in the VM to print the error and reboot, or halt after the error is printed. Extreme caution
should be used when changing any non AtomVM settings. You can quit the program by typing `Q`.
Save the changes, and the program will exit.
```

You can now build AtomVM using the build command:

```shell
$ idf.py build
```

This command, once completed, will create the Espressif bootloader, partition table, and AtomVM binary.  The last line of the output should read something like the following:

    Project build complete. To flash, run this command:
    ~/.espressif/python_env/idf5.1_py3.11_env/bin/python ~/esp/esp-idf-v5.1/components
    /esptool_py/esptool/esptool.py -p (PORT) -b 921600 --before default_reset
    --after hard_reset --chip esp32 write_flash --flash_mode dio --flash_size detect
    --flash_freq 40m 0x1000 build/bootloader/bootloader.bin 0x8000
    build/partition_table/partition-table.bin 0x10000 build/atomvm-esp32.bin
    or run 'idf.py -p (PORT) flash'

At this point, you can run `idf.py flash` to upload the 3 binaries up to your ESP32 device, and in some development scenarios, this is a preferable shortcut.

However, first, we will build a single binary image file containing all of the above 3 binaries, as well as the AtomVM core libraries.  See [Building a Release Image](#building-a-release-image), below.  But first, it is helpful to understand a bit about how the AtomVM partitioning scheme works, on the ESP32.

### Running tests for ESP32

Tests for ESP32 are run on the desktop (or CI) using qemu.

Install or compile [Espressif's fork of qemu](https://github.com/espressif/esp-toolchain-docs/blob/main/qemu/README.md).
Espressif provides [binaries for Linux amd64](https://github.com/espressif/qemu/releases) and it's also bundled in [espressif/idf:5.1 docker image](https://hub.docker.com/r/espressif/idf).

Also install Espressif pytest's extensions for embedded testing with:

```shell
$ cd <ESP-IDF-ROOT-DIR>
$ . ./export.sh
$ pip install pytest==7.0.1 \
    pytest-embedded==1.2.5 \
    pytest-embedded-serial-esp==1.2.5 \
    pytest-embedded-idf==1.2.5 \
    pytest-embedded-qemu==1.2.5
```

Change directory to the `src/platforms/esp32/test` directory under the AtomVM source tree root:

```shell
$ cd <atomvm-source-tree-root>
$ cd src/platforms/esp32/test
```

Build tests using the build command:

```shell
$ idf.py build
```

```{note}
This eventually compiles host AtomVM to be able to build and pack erlang test modules.
```

Run tests using the command:

```shell
$ pytest --embedded-services=idf,qemu -s
```

ESP32 tests are erlang modules located in `src/platforms/esp32/test/main/test_erl_sources/` and executed from `src/platforms/esp32/test/main/test_main.c`.

### Performance and Power

AtomVM comes with conservative defaults for broad compatibility with different ESP32 boards, and a reasonable performance/power/longevity tradeoff.

You may want to change these settings to optimize for your specific application's performance and power needs.

Factors like heat dissipation should also be considered, and the effect on overall longevity of components.

#### CPU frequency

Use `idf.py menuconfig` in `src/platforms/esp32`
`Component config ---> ESP System Settings  ---> CPU frequency (160 MHz)  --->`

You can increase or decrease the CPU frequency, this is a tradeoff against power usage.
Eg. 160 MHz is the conservative default for the ESP32, but you can increase it to 240 MHz or decrease it to 80 MHz. The higher the frequency, the more power is consumed. The lower the frequency, the less power is consumed.

#### Flash mode and speed

Use `idf.py menuconfig` in `src/platforms/esp32`
`Serial flasher config  ---> Flash SPI mode (DIO)  --->`
You can change the mode of the SPI flash. QIO is the fastest mode, but not all flash chips support it.

`Serial flasher config  ---> Flash SPI speed (40 MHz)  --->`
You can change the speed of the SPI flash. The higher the speed, the faster the flash will be, at the cost of higher power usage, but not all flash chips support higher speeds.

See external docs: [ESP-IDF flash modes](https://docs.espressif.com/projects/esptool/en/latest/esp32/advanced-topics/spi-flash-modes.html)

#### PSRAM speed

Use `idf.py menuconfig` in `src/platforms/esp32`
`Component config ---> ESP PSRAM  ---> SPI RAM config  --->`

If your board has PSRAM and it's enabled, you can configure the SPI RAM settings here.
`Set RAM clock speed (40MHz clock speed)  --->`
You can increase or decrease the clock speed of the PSRAM.

```{warning}
You may have to increase "Flash SPI speed" (see above) before you can increase PSRAM speed.
```

The higher the speed, the faster the PSRAM will be, at the cost of higher power usage, but not all PSRAM chips support higher speeds.

#### Sleep mode - Deep sleep

For low power applications, you should use the [deep sleep functionality](./programmers-guide.md#restart-and-deep-sleep) of the ESP32.

This will put the ESP32 into a very low power state, and it will consume very little power.
You can wake the ESP32 from deep sleep using a timer, or an interrupt etc.

Make sure your board is suitable for low power deep sleep, some boards have voltage regulators and/or LEDs constantly draining power, also make sure sensors are powered down or in low power mode when the ESP32 is in deep sleep.

For persisting small amounts of data during deep sleep, you can use the [RTC memory](./programmers-guide.md#rtc-memory) of the ESP32, which is preserved during deep sleep.

#### Sleep mode - Light sleep

Usage of light sleep is untested, and no support for controlling light sleep is currently implemented. Reach out if you do any experiments and measurements.

#### Other Build settings

Various other build settings can be changed in `idf.py menuconfig` in `src/platforms/esp32`, that affect the performance and power usage of the ESP32.

Of note AtomVM releases are built with the `Optimize for performance (-O2)` compiler option:
Use `idf.py menuconfig` in `src/platforms/esp32`
`Compiler options ---> Optimization Level () --->`

See [CONFIG_COMPILER_OPTIMIZATION_PERF](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/kconfig-reference.html#config-compiler-optimization)

And all builds are built with the mbedTLS `Enable fixed-point multiplication optimisations` option:
Use `idf.py menuconfig` in `src/platforms/esp32`
`Component config ---> mbedTLS  ---> Enable fixed-point multiplication optimisations`

See [MBEDTLS_ECP_FIXED_POINT_OPTIM](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/kconfig-reference.html#config-mbedtls-ecp-fixed-point-optim)

### Flash Layout

The AtomVM Flash memory is partitioned to include areas for the above binary artifacts created from the build, as well areas for runtime information used by the ESP32 and compiled Erlang/Elixir code.

The flash layout is roughly as follows (not to scale):

    +-----------------+  ------------- 0x0 | 0x1000 | 0x2000
    |                 |             ^
    |   boot loader   | 28KB        |
    |                 |             |
    +-----------------+             |
    | partition table | 3KB         |
    +-----------------+             |
    |                 |             |
    |       NVS       | 24KB        |
    |                 |             |
    +-----------------+             |
    |     PHY_INIT    | 4KB         |
    +-----------------+             | AtomVM
    |                 |             | binary
    |                 |             | image
    |                 |             |
    |     AtomVM      |             |
    |     Virtual     | 1.75MB      |
    |     Machine     |             |
    |                 |             |
    |                 |             |
    +-----------------+             |
    |     boot.avm    | 256-512KB   v
    +-----------------+  ------------- 0x210000 for Erlang only images or
    |                 |             ^  0x250000 for images with Elixir modules
    |                 |             |
    |     main.avm    | 1MB+        | Erlang/Elixir
    |                 |             | Application
    |                 |             |
    |                 |             v
    +-----------------+  ------------- end

The following table summarizes the partitions created on the ESP32 when deploying AtomVM:

| Partition | Offset | Length | Description |
|:----------|:-------|:-------|:------------|
| Bootloader | 0x0 \| 0x1000 \| 0x2000 | 28kB | The ESP32 bootloader, as built from the IDF-SDK.  AtomVM does not define its own bootloader. The offset of the bootloader varies by chip.|
| Partition Table | 0x8000 | 3kB | The AtomVM-defined partition table. |
| NVS | 0x9000 | 24kB | Space for non-volatile storage. |
| PHY_INIT | 0xF000 | 4kB | Initialization data for physical layer radio signal data. |
| AtomVM virtual machine | 0x10000 | 1.75mB | The AtomVM virtual machine (compiled from C code). |
| boot.avm | 0x1D0000 | 256k | The AtomVM BEAM library, compiled from Erlang and Elixir files in the AtomVM source tree. |
| main.avm | `0x210000` \| `0x250000` | 1mB | The user application.  This is where users flash their compiled Erlang/Elixir code |

```{warning}
There is an important difference in the partition layout between the minimal images and those build with Elixir support. To accommodate the extra Elixir modules the boot.avm partition on these images is larger, and the application offset is moved accordingly. When working with Elixir supported images it is important to always use the offset `0x250000` whether using `mix` or the `atomvm_rebar3_plugin` (possibly to test an Erlang app), otherwise part of the boot.avm partition (specifically the area where many Elixir modules are located) will be overwritten with the application, but the VM will still be trying to load from the later `0x250000` offset. This should be kept in mind reading the rest of build instructions, and [AtomVM Tooling](./atomvm-tooling.md) sections of the docs that cover the use of rebar3, for these sections an Erlang only image is assumed.
```

### The `boot.avm` and `main.avm` partitions

The `boot.avm` and `main.avm` partitions are intended to store Erlang/Elixir libraries (compiled down to BEAM files, and assembled as AVM files).

The `boot.avm` partition is intended for core Erlang/Elixir libraries that are built as part of the AtomVM build.  The release image of AtomVM (see below) includes both the AtomVM virtual machine and the `boot.avm` partition, which includes the BEAM files from the `estdlib` and `eavmlib` libraries.

In contrast, the `main.avm` partition is intended for user applications.  Currently, the `main.avm` partition starts at address `0x210000` for thin images or `0x250000` for images with Elixir modules, and it is to that location to which application developers should flash their application AVM files.

The AtomVM search path for BEAM modules starts in the `main.avm` partition and falls back to `boot.avm`.  Users should not have a need to override any functionality in the `boot.avm` partition, but if necessary, a BEAM module of the same name in the `main.avm` partition will be loaded instead of the version in the `boot.avm` partition.

```{warning}
The location of the `main.avm` partition may change over time, depending on the relative sizes of the AtomVM binary
and `boot.avm` partitions.
```

### Building a Release Image

The `<atomvm-source-tree-root>/tools/release/esp32` directory contains the `mkimage.sh` script that can be used to create a single AtomVM image file, which can be distributed as a release, allowing application developers to develop AtomVM applications without having to build AtomVM from scratch.

```{attention}
Before running the `mkimage.sh` script, you must have a complete build of both the esp32 project, as well as a full
build of the core Erlang libraries in the `libs` directory.  The script configuration defaults to assuming that the
core Erlang libraries will be written to the `build/libs` directory in the AtomVM source tree.  You should pass the
`--build_dir <path>` option to the `mkimage.sh` script, with `<path>` pointing to your AtomVM build directory, if
you target a different build directory when running CMake.
```

Running this script will generate a single `atomvm-<sha>.img` file in the `build` directory of the esp32 source tree, where `<sha>` is the git hash of the current checkout.  This image contains the ESP32 bootloader, AtomVM executable, and the `eavmlib` and `estdlib` Erlang libraries in one file, which can then be flashed to address `0x1000` for the esp32. The bootloader address varies for other chip variants. See the [flashing a binary image to ESP32](./getting-started-guide.md#flashing-a-binary-image-to-esp32) section of the [Getting Started Guide](./getting-started-guide.md) for a chart with the bootloader offset address of each model.

To build a thin image with only Erlang libraries `mkimage.sh` script is run from the `src/platform/esp32` directory as follows:

```shell
$ ./build/mkimage.sh
Writing output to /home/joe/AtomVM/src/platforms/esp32/build/atomvm-esp32.img
=============================================
Wrote bootloader at offset 0x1000 (4096)
Wrote partition-table at offset 0x8000 (32768)
Wrote AtomVM Virtual Machine at offset 0x10000 (65536)
Wrote AtomVM Core BEAM Library at offset 0x1D0000 (1114112)
```

To build a full image with Erlang and Elixir libraries the path to the previously (during the generic_unix build) built `elixir_esp32boot.avm` must be passed to the `mkimage.sh` script as follows (Note: this is still run from the AtomVM/src/platforms/esp32 directory for the relative path to work - feel free to use the absolute path to this file):

```shell
$ ./build/mkimage.sh --boot ../../../build/libs/esp32boot/elixir_esp32boot.avm
Writing output to /home/joe/AtomVM/src/platforms/esp32/build/atomvm-esp32.img
=============================================
Wrote bootloader at offset 0x1000 (4096)
Wrote partition-table at offset 0x8000 (32768)
Wrote AtomVM Virtual Machine at offset 0x10000 (65536)
Wrote AtomVM Core BEAM Library at offset 0x1D0000 (1114112)
```

Users can then use the `esptool.py` directly to flash the entire image to the ESP32 device, and then flash their applications to the `main.app` partition at address `0x210000`, (or `0x250000` for Elixir images)

But first, it is a good idea to erase the flash, e.g.,

```shell
$ esptool.py --chip esp32 --port /dev/ttyUSB0 erase_flash
esptool.py v2.1
Connecting........_
Chip is ESP32D0WDQ6 (revision 1)
Uploading stub...
Running stub...
Stub running...
Erasing flash (this may take a while)...
Chip erase completed successfully in 5.4s
Hard resetting...
```

#### Flashing Release Images

After preparing a release image you can use the `flashimage.sh`, which is generated with each build that will flash the full image using the correct flash offset for the chip the build was configured for using the either the default Erlang only `partitions.cvs` table, or the `partitions-elixir.cvs` table if that was used during the configuration.

```shell
$ ./build/flashimage.sh
```

To perform this action manually you can use the `./build/flash.sh` tool (or `esptool.py` directly, if you prefer):

```shell
$ FLASH_OFFSET=0x1000 ./build/flash.sh ./build/atomvm-esp32-0.6.6.img
esptool.py v2.8-dev
Serial port /dev/tty.SLAB_USBtoUART
Connecting........_
Chip is ESP32D0WDQ6 (revision 1)
Features: WiFi, BT, Dual Core, Coding Scheme None
Crystal is 40MHz
MAC: 30:ae:a4:1a:37:d8
Uploading stub...
Running stub...
Stub running...
Changing baud rate to 921600
Changed.
Configuring flash size...
Auto-detected Flash size: 4MB
Wrote 1163264 bytes at 0x00001000 in 15.4 seconds (603.1 kbit/s)...
Hash of data verified.
Leaving...
Hard resetting via RTS pin...
```

```{caution}
Flashing the full AtomVM image will delete all entries in non-volatile storage.  Only flash the full image if you
have a way to recover and re-write any such data, if you need to retain it.
```

### Flashing Applications

Applications can be flashed using the `flash.sh` script in the esp32 build directory (the application offset is set
correctly depending on the build configuration):

```shell
$ ./build/flash.sh ../../../build/examples/erlang/esp32/blink.avm
%%
%% Flashing examples/erlang/esp32/blink.avm (size=4k)
%%
esptool.py v2.8-dev
Serial port /dev/tty.SLAB_USBtoUART
Connecting........_
Chip is ESP32D0WDQ6 (revision 1)
Features: WiFi, BT, Dual Core, Coding Scheme None
Crystal is 40MHz
MAC: 30:ae:a4:1a:37:d8
Uploading stub...
Running stub...
Stub running...
Changing baud rate to 921600
Changed.
Configuring flash size...
Auto-detected Flash size: 4MB
Wrote 16384 bytes at 0x00210000 in 0.2 seconds (611.7 kbit/s)...
Hash of data verified.
Leaving...
Hard resetting via RTS pin...
```

```{tip}
Since the Erlang core libraries are flashed to the ESP32 device, it is not necessary to include core libraries in
your application AVM files.  Users may be interested in using downstream development tools, such as the Elixir
[ExAtomVM Mix task](https://github.com/atomvm/ExAtomVM), or the Erlang
[AtomVM Rebar3 Plugin](https://github.com/atomvm/atomvm_rebar3_plugin) for doing day-to-day development of
applications for the AtomVM platform.
```

#### Flashing the core libraries

If you are doing development work on the core Erlang/Elixir libraries and wish to test changes that do not involve the `C` code in the core VM you may flash `esp32boot.avm` (or `elixir_esp32boot.avm` when using an Elixir partition table) to the boot.avm partition (offset 0x1D0000) by using the `flash.sh` script in the esp32 build directory as follows:

```shell
$ build/flash.sh -l ../../../build/libs/esp32boot.avm
%%
%% Flashing ../../../build/libs/esp32boot.avm (size=116k)
%%
esptool.py v4.5.1
Serial port /dev/ttyUSB0
Connecting.....
Detecting chip type... Unsupported detection protocol, switching and trying
again...
Connecting.....
Detecting chip type... ESP32
Chip is ESP32-D0WD (revision v1.0)
Features: WiFi, BT, Dual Core, 240MHz, VRef calibration in efuse, Coding Scheme
None
Crystal is 40MHz
MAC: 1a:57:c5:7f:ac:5b
Uploading stub...
Running stub...
Stub running...
Changing baud rate to 921600
Changed.
Configuring flash size...
Auto-detected Flash size: 8MB
Flash will be erased from 0x001d0000 to 0x001ecfff...
Wrote 131072 bytes at 0x001d0000 in 1.8 seconds (582.1 kbit/s)...
Hash of data verified.

Leaving...
Hard resetting via RTS pin...
```

### Adding custom Nifs, Ports, and third-party components

While AtomVM is a functional implementation of the Erlang virtual machine, it is nonetheless designed to allow developers to extend the VM to support additional integrations with peripherals and protocols that are not otherwise supported in the core virtual machine.

AtomVM supports extensions to the VM via the implementation of custom native functions (Nifs) and processes (AtomVM Ports), allowing users to extend the VM for additional functionality, and you can add your own custom Nifs, ports, and additional third-party components to your ESP32 build by adding them to the `components` directory, and the ESP32 build will compile them automatically.

```{seealso}
For more information about building components for the IDF SDK, consult the
[IDF SDK Build System](https://docs.espressif.com/projects/esp-idf/en/v5.1.3/esp32/api-guides/build-system.html)
documentation.
```

The instructions for adding custom Nifs and ports differ in slight detail, but are otherwise quite similar.  In general, they involve:

1. Adding the custom Nif or Port to the `components` directory of the AtomVM source tree.
1. Run `idf.py reconfigure` to pick up any menuconfig options, many extra drivers have an option to disable them (they are enabled by default). Optionally use `idf.py menuconfig` and confirm the driver is enabled and save when quitting.
1. Building the AtomVM binary.

```{attention}
The Espressif SDK and tool chains do not, unfortunately, support dynamic loading of shared libraries and dynamic
symbol lookup.  In fact, dynamic libraries are not supported at all on the ESP32 using the IDF SDK; instead, any
code that is needed at runtime must be statically linked into the application.
```

Custom Nifs and Ports are available through third parties.  Follow the instructions provided with these custom components for detailed instruction for how to add the Nif or Port to your build.

More detailed instructions follow, below, for implementing your own Nif or Port.

#### Adding a custom AtomVM Nif

To add support for a new peripheral or protocol using custom AtomVM Nif, you need to do the following:

* Choose a name for your nif (e.g, "my_nif").  Call this `<moniker>`.
* In your source code, implement the following two functions:
  * `void <moniker>_nif_init(GlobalContext *global);`
    * This function will be called once, when the application is started.
  * `const struct Nif *<moniker>_nif_get_nif(const char *nifname);`
    * This function will be called to locate the Nif during a function call.
    Example:

    ```c
        void my_nif_init(GlobalContext *global);
        const struct Nif *my_nif_get_nif(const char *nifname);
    ```

    ```{note}
    Instructions for implementing Nifs is outside of the scope of this document.
    ```

* Add the `REGISTER_NIF_COLLECTION` using the parameters `NAME`, `INIT_CB`, `DESTROY_CB`, `RESOLVE_NIF_CB` macro to the end of your nif code. Example:

    ```c
        REGISTER_NIF_COLLECTION(my_nif, NULL, NULL, my_nif_get_nif);
    ```


#### Adding a custom AtomVM Port

To add support for a new peripheral or protocol using an AtomVM port, you need to do the following:

* Choose a name for your port (e.g, "my_port").  Call this `<moniker>`.
* In your source code, implement the following two functions:
  * `void <moniker>_init(GlobalContext *global);`
    * This function will be called once, when the application is started.
  * `Context *<moniker>_create_port(GlobalContext *global, term opts);`
    * This function will be called to locate the Nif during a function call.
    Example:

    ```c
        void my_port_init(GlobalContext *global);
        Context *my_port_create_port(GlobalContext *global, term opts);
    ```

    ```{note}
    Instructions for implementing Ports is outside of the scope of this document.
    ```

* Add the `REGISTER_PORT_COLLECTION` using the parameters `NAME`, `INIT_CB`, `DESTROY_CB`, `RESOLVE_NIF_CB` macro to the end of your nif code. Example:

    ```c
        REGISTER_PORT_COLLECTION(my_port, my_port_init, NULL, my_port_create_port);
    ```


## Building for STM32

### STM32 Prerequisites

The following software is required to build AtomVM for the STM32 platform:

* [11.3 ARM toolchain](https://developer.arm.com/-/media/Files/downloads/gnu/11.3.rel1/binrel/arm-gnu-toolchain-11.3.rel1-x86_64-arm-none-eabi.tar.xz) (or compatible with your system)
* [libopencm3](https://github.com/libopencm3/libopencm3.git) version 0.8.0
* `cmake`
* `make`
* `git`
* `python`
* Erlang/OTP `escript`

```{note}
AtomVM tests this build on the latest Ubuntu github runner.
```

### Setup libopencm3

Before building for the first time you need to have a compiled clone of the libopencm3 libraries, from inside the AtomVM/src/platforms/stm32 directory:

```shell
$ git clone -b v0.8.0 https://github.com/libopencm3/libopencm3.git
$ cd libopencm3 && make -j4 && cd ..
```

```{tip}
You can put libopencm3 wherever you want on your PC as long as you update LIBOPENCM3_DIR to point to it. This
example assumes it has been cloned into /opt/libopencm3 and built. From inside the AtomVM/src/platforms/stm32
directory:

    $ cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/arm-toolchain.cmake \
    -DLIBOPENCM3_DIR=/opt/libopencm3 ..

```

### Build AtomVM with cmake toolchain file

```shell
$ mkdir build
$ cd build
$ cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/arm-toolchain.cmake ..
$ make
```

### Changing the target device

The default build is based on the STM32F4Discovery board chip (`stm32f407vgt6`). If you want to target a different
chip, pass the `-DDEVICE` flag when invoking cmake. For example, to use the BlackPill V2.0, pass `-DDEVICE=stm32f411ceu6`. At this time any `STM32F4` or `STM32F7` device with 512KB or more of on package flash should work with AtomVM. If an unsupported device is passed with the `DEVICE` parameter the configuration will fail. For devices with either 512KB or 768KB of flash the available application flash space will be limited to 128KB. Devices with only 512KB of flash may also suffer from slightly reduced performance because the compiler must optimize for size rather than performance.

```{attention}
For devices with only 512KB of flash the application address is different and must be adjusted when flashing your
application with st-flash, or using the recommended `atomvm_rebar3_plugin`. The application address for these
devices is `0x8060000`.
```

### Configuring the Console

The default build for any `DEVICE` will use `USART2` and output will be on `PA2`. This default will work well for most `Discovery` and generic boards that do not have an on-board TTL to USB-COM support (including the `stm32f411ceu6` A.K.A. `BlackPill V2.0`). For `Nucleo` boards that do have on board UART to USB-COM support you may pass the `cmake` parameter `-DBOARD=nucleo` to have the correct USART and TX pins configured automatically. The `Nucleo-144` series use `USART3` and `PD8`, while the supported `Nucleo-64` boards use `USART2`, but passing the `BOARD` parameter along with `DEVICE` will configure the correct `USART` for your model. If any other boards are discovered to have on board USB UART support pull requests, or opening issues with the details, are more than welcome.

Example to configure a `NUCLEO-F429ZI`:

```shell
$ cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/arm-toolchain.cmake -DDEVICE=stm32f429zit6 \
-DBOARD=nucleo
```

The AtomVM system console `USART` may also be configured to a specific uart peripheral. Pass one of the parameters from the chart below with the `cmake` option `-DAVM_CFG_CONSOLE=CONSOLE_#`, using the desired console parameter in place of `CONSOLE_#`. Not all UARTs are available on every supported board, but most will have several options that are not already used by other on board peripherals. Consult your data sheets for your device to select an appropriate console.

| Parameter | USART | TX Pin | AtomVM Default | Nucleo-144 | Nucleo-64 |
|-----------|-------|--------|----------------|------------|-----------|
| `CONSOLE_1` | `USART1` | `PA9` |   |   |   |
| `CONSOLE_2` | `USART2` | `PA2` | ✅ |   | ✅ |
| `CONSOLE_3` | `USART3` | `PD8` |   | ✅ |   |
| `CONSOLE_4` | `UART4` | `PC10` |   |   |   |
| `CONSOLE_5` | `UART5` | `PC12` |   |   |   |
| `CONSOLE_6` | `USART6` | `PC6` |   |   |   |
| `CONSOLE_7` | `UART7` | `PF7` |   |   |   |
| `CONSOLE_8` | `UART8` | `PJ8` |   |   |   |

### Configure STM32 logging with `cmake`

The default maximum log level is `LOG_INFO`. To change the maximum level displayed pass `-DAVM_LOG_LEVEL_MAX="{level}"` to `cmake`, with one of `LOG_ERROR`, `LOG_WARN`, `LOG_INFO`, or `LOG_DEBUG` (listed from least to most verbose). Log messages can be completely disabled by using `-DAVM_LOG_DISABLE=on`.

For log entries colorized by log level pass `-DAVM_ENABLE_LOG_COLOR=on` to cmake. With color enable there is a very small performance penalty (~1ms per message printed), the log entries are colored as follows:

| Message Level |  Color  |
|---------------|---------|
| ERROR | Red |
| WARN | Orange |
| INFO | Green |
| DEBUG | Blue |

By default only `ERROR` messages contain file and line number information. This can be included with all log entries by passing `-DAVM_ENABLE_LOG_LINES=on` to cmake, but it does incur a significant performance penalty and is only suggested for debugging during development.

### Console Printing on STM32

AtomVM is built with standard `newlib` to support `long long` integers (`signed` and `unsigned`). If you are building for a device with extremely limited flash space the `nano` version of `newlib` can be used instead. This may be done by passing `-DAVM_NEWLIB_NANO=on`. If the `nano newlib` is used logs will be automatically disabled, this is because many of the VM low level log messages will include `%ull` formatting and will cause buffer overflows and crash the VM if logging is not disabled for `nano newlib` builds. The total flash savings of using `nano newlib` and disabling logs is just under 40kB.

By default, stdout and stderr are printed on USART2. On the STM32F4Discovery board, you can see them
using a TTL-USB with the TX pin connected to board's pin PA2 (USART2 RX). Baudrate is 115200 and serial transmission
is 8N1 with no flow control.

```{seealso}
If building for a different target USART may be configure as explained above in
[Configuring the Console](#configuring-the-console).
```

### Configuring deployment builds for STM32

After your application has been tested (_and debugged_) and is ready to put into active use you may want to tune the build of AtomVM.  For instance disabling logging with `-DAVM_LOG_DISABLE=on` as a `cmake` configuration option may result in slightly better performance. This will have no affect on the console output of your application, just disable low level log messages from the AtomVM system. You may also want to enabling automatic reboot in the case that your application ever exits with a return other than `ok`. This can be enabled with the `cmake` option `-DAVM_CONFIG_REBOOT_ON_NOT_OK=on`.

## Building for Raspberry Pi RP2

You can build with all boards supported by Raspberry Pi pico SDK, including Pico, Pico W, Pico2 and Pico 2 W. AtomVM also works with clones such as RP2040 Zero.

### RP2 Prerequisites

* `cmake`
* `ninja`
* `Erlang/OTP`
* `Elixir` (optional)
* A toolchain for the target (ARM or Risc-V)

### AtomVM build steps (Pico or most boards based on RP2040)

```shell
$ cd src/platforms/rp2/
$ mkdir build
$ cd build
$ cmake .. -G Ninja
$ ninja
```

```{tip}
You may want to build with option `AVM_REBOOT_ON_NOT_OK` so AtomVM restarts on error.
```

### AtomVM build steps (Pico W)

```shell
$ cd src/platforms/rp2/
$ mkdir build
$ cd build
$ cmake .. -G Ninja -DPICO_BOARD=pico_w
$ ninja
```

```{tip}
You may want to build with option `AVM_REBOOT_ON_NOT_OK` so AtomVM restarts on error.
```

### AtomVM build steps (Pico 2 or boards based on RP2350)

For ARM S platform (recommended) :
```shell
$ cd src/platforms/rp2/
$ mkdir build
$ cd build
$ cmake .. -G Ninja -DPICO_BOARD=pico2
$ ninja
```

For RISC-V platform (supported but slower) :

```shell
$ cd src/platforms/rp2/
$ mkdir build
$ cd build
$ cmake .. -G Ninja -DPICO_BOARD=pico2 -DPICO_PLATFORM=rp2350-riscv
$ ninja
```

```{tip}
You may want to build with option `AVM_REBOOT_ON_NOT_OK` so AtomVM restarts on error.
```

### AtomVM build steps (Pico 2 W)

For ARM S platform (recommended) :
```shell
$ cd src/platforms/rp2/
$ mkdir build
$ cd build
$ cmake .. -G Ninja -DPICO_BOARD=pico2_w
$ ninja
```

For RISC-V platform (supported but slower) :

```shell
$ cd src/platforms/rp2/
$ mkdir build
$ cd build
$ cmake .. -G Ninja -DPICO_BOARD=pico2_w -DPICO_PLATFORM=rp2350-riscv
$ ninja
```

```{tip}
You may want to build with option `AVM_REBOOT_ON_NOT_OK` so AtomVM restarts on error.
```

The default build configuration allows the device to be re-flashed with the `atomvm_rebar3_plugin` `atomvm pico_flash` task or restarting the application after exiting using [`picotool`](https://github.com/raspberrypi/picotool).  This behaviour can be changed to hang the CPU when the application exits, so that power must be cycled to restart, and `BOOTSEL` must be held when power on to flash a new application.  To disable software resets use `-DAVM_WAIT_BOOTSEL_ON_EXIT=off` when configuring `cmake`.

The 20 second default timeout for a USB serial connection can be changed using option `AVM_USB_WAIT_SECONDS`.  The device can also be configured to wait indefinitely for a serial connection using the option `AVM_WAIT_FOR_USB_CONNECT=on`.

### libAtomVM build steps for RP2

Build of standard libraries is part of the generic unix build.

From the root of the project:

```shell
$ mkdir build
$ cd build
$ cmake .. -G Ninja
$ ninja
```

### Running tests for RP2

Tests for RP2040 are run on the desktop (or CI) using [rp2040js](https://github.com/wokwi/rp2040js).
Running tests currently require nodejs 20.

Change directory to the `src/platforms/rp2/tests` directory under the AtomVM source tree root:

```shell
$ cd <atomvm-source-tree-root>
$ cd src/platforms/rp2/tests
$
```

Install the emulator and required Javascript dependencies:

```shell
$ npm install
```

We are assuming tests were built as part of regular build of AtomVM. Run them with the commands:

```shell
$ npx tsx run-tests.ts ../build/tests/rp2040_tests.uf2 \
../build/tests/test_erl_sources/rp2040_test_modules.uf2
```

## Building for `emscripten`

Two different builds are possible, depending on link options: for NodeJS and
for the web browser.

### WASM Prerequisites

* [emscripten SDK](https://emscripten.org)
* `cmake`
* Erlang/OTP
* Elixir (optional)

### Building for NodeJS

This is the default. Execute the following commands:

```shell
$ cd src/platforms/emscripten/
$ mkdir build
$ cd build
$ emcmake cmake ..
$ emmake make -j
```

AtomVM can then be invoked as on Generic Unix with node:

```shell
$ node ./src/AtomVM.js
```

### Running tests with NodeJS

NodeJS build currently does not have dedicated tests. However, you can run
AtomVM library tests that do not depend on unimplemented APIs.

Build them first by building AtomVM for Generic Unix (see above.)
Then execute the tests with:

```shell
$ cd src/platforms/emscripten/build/
$ node ./src/AtomVM.js ../../../../build/tests/libs/eavmlib/test_eavmlib.avm
$ node ./src/AtomVM.js ../../../../build/tests/libs/alisp/test_alisp.avm
```

### Building for the web

Execute the following commands:

```shell
$ cd src/platforms/emscripten/
$ mkdir build
$ cd build
$ emcmake cmake .. -DAVM_EMSCRIPTEN_ENV=web
$ emmake make -j
```

### Running tests with Cypress

AtomVM WebAssembly port on the web uses SharedArrayBuffer feature which is
restricted by browsers. Tests require an HTTP server that returns the proper
HTTP headers.

Additionally, tests require [Cypress](https://www.cypress.io). Plus, because of
a current [bug in Cypress](https://github.com/cypress-io/cypress/issues/19912),
tests only run with Chrome-based browsers except Electron (Chromium, Chrome or Edge).

Build first AtomVM for Generic Unix (see above). This will include the web
server.

Then run the web server with:

```shell
$ cd build
$ ./src/AtomVM examples/emscripten/wasm_webserver.avm
```

In another terminal, compile specific test modules that are not part of examples.

```shell
$ cd src/platforms/emscripten/build/
$ make emscripten_erlang_test_modules
```

Then run tests with Cypress with:

```shell
$ cd src/platforms/emscripten/tests/
$ npm install cypress
$ npx cypress run --browser chrome
```

You can alternatively specify: `chromium` or `edge` depending on what is installed.

Alternatively, on Linux, you can run tests with docker:

```shell
$ cd src/platforms/emscripten/tests/
$ docker run --network host -v $PWD:/mnt -w /mnt cypress/included:12.17.1 \
--browser chrome
```

Or you can open Cypress to interactively run selected test suites.

```shell
$ cd src/platforms/emscripten/tests/
$ npm install cypress
$ npx cypress open
```
