<!--
 Copyright 2021-2022 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Build Instructions

This guide is intended for anyone interested in the implementation of AtomVM, including developers who would like to provide enhancements or bug fixes to the core AtomVM virtual machine, as well as providing third-party extensions via the implementation of AtomVM Nifs or Ports.  Understanding the virtual machine and how it is implemented is also interesting in its own right, and having an understanding of the underlying mechanics can be helpful for writing better Erlang and Elixir programs.

The AtomVM virtual machine itself, including the runtime code execution engine, as well as built-in functions and Nifs is implemented in C.  The core standard and AtomVM libraries are implemented in Erlang and Elixir.

The native C parts of AtomVM compile to machine code on MacOS, Linux, and FreeBSD platforms.  The C code also compiles to run on the ESP32 and STM32 platforms.  Typically, binaries for these platforms are created on a UNIX-like environment (MacOS or Linux, currently) using tool-chains provided by device vendors to cross-compile and target specific device architectures.

The Erlang and Elixir parts are compiled to BEAM byte-code using the Erlang (`erlc`) and Elixir compilers.  Currently, Erlang/OTP versions 21 and 22 are supported.

This guide provides information about how to build AtomVM for the various supported platforms (Generic UNIX, ESP32, and STM32).

> Note.  In order to build AtomVM AVM files for ESP32 and STM32 platforms, you will also need to build AtomVM for the Generic UNIX platform of your choice.

## Downloading AtomVM

The AtomVM source code is available by cloning the AtomVM github repository:

	shell$ git clone https://github.com/atomvm/AtomVM

> Note.  Downloading the AtomVM github repository requires the installation of the `git` program.  Consult your local OS documentation for installation of the `git` package.

## Source code organization

Source code is organized as follows:

* `src`  Contains the core AtomVM virtual machine source code;
* `lib`  Contains the Erlang and Elixir core library source code;
* `tools` Contains AtomVM tooling, including the `PackBEAM` executable, as well as build support tooling;
* `examples` Contains sample programs for demonstration purposes;
* `tests` Contains test code run as part of test qualification;
* `doc` Contains documentation source code and content.

The `src` directory is broken up into the core platform-independent AtomVM library (`libAtomVM`), and platform-dependent code for each of the supported platforms  (Generic UNIX, ESP32, and STM32).

For information about porting to new platforms, see [Porting to new platforms](#porting-to-new-platforms), below.

## Building for Generic UNIX

The following instructions apply to unix-like environments, including Linux, FreeBSD, and MacOS.

> Note.  The Generic UNIX is useful for running and testing simple AtomVM programs.  Not all of the AtomVM APIs, specifically, APIs that are dependent on various device integration, are supported on this platform.

### Build Requirements

The following software is required in order to build AtomVM in generic UNIX systems:

* `gcc` or `llvm` tool chains
* `cmake`
* `make`
* `gperf`
* `zlib`
* Erlang/OTP 21

Consult your local OS documentation for instructions about how to install these components.

### Build Instructions

The AtomVM build for generic UNIX systems makes use of the `cmake` tool for generating `make` files from the top level AtomVM directory.  With CMake, you generally create a separate directory for all output files (make files, generated object files, linked binaries, etc).  A common pattern is to create a clcal `build` directory, and then point `cmake` to the parent directory for the root of the source tree:

	shell$ mkdir build
	shell$ cd build
	shell$ cmake ..
	...

This command will create all of the required make files for creating the AtomVM binary, tooling, and core libraries.  You can create all of these object using the `make` command:

	shell$ make -j 8
	...

> Note.  You may specify `-j <n>`, where `<n>` is the number of CPUs you would like to assign to run the build in parallel.

Upon completion, the `AtomVM` executable can be found in the `build/src` directory.

The AtomVM core Erlang library can be found in the generated `libs/atomvmlib.avm` AVM file.

#### Special Note for MacOS users

You may build an Apple Xcode project, for developing, testing, and debugging in the Xcode IDE, by specifying the Xcode generator.  For example, from the top level AtomVM directory:

	shell$ mkdir xcode
	shell$ cmake -G Xcode ..
	...
	shell$ open AtomVM.xcodeproj

The above commands will build and open an AtomVM project in the Xcode IDE.

### Running tests

There are currently two sets of suites of tests for AtomVM:

* Erlang tests (`erlang_tests`) A set of unit tests for basic Erlang functionality, exercising support BEAM opcodes, built-in functions (Bifs) and native functions (Nifs).
* Library tests, exercising functionality in the core Erlang and Elixir libraries.

To run the Erlang tests, run the `test-erlang` executable in the `tests` directory:

	shell$ ./tests/test-erlang

This will run a suite of several score unit tests.  Check the status of the executable after running the tests.  A non-zero return value indicates a test failure.

To run the Library tests, run the corresponding AVM module in the `tests/libs` directory using the `AtomVM` executable.  For example:

	shell$ ./src/AtomVM ./tests/libs/estdlib/test_estdlib.avm

This will run a suite of several unit tests for the specified library.  Check the status of the executable after running the tests.  A non-zero return value indicates a test failure.

Tests for the following libraries are supported:

* `estdlib`
* `eavmlib`
* `alisp`

## Building for ESP32

Building AtomVM for ESP32 must be done on either a Linux or MacOS build machine.

In order to build a complete AtomVM image for ESP32, you will also need to build AtomVM for the Generic UNIX platform (typically, the same build machine you are suing to build AtomVM for ESP32).

### Build Requirements

The following software is required in order to build AtomVM for the ESP32 platform:

* Espressif Xtensa tool chains
* [Espressif IDF SDK](https://www.espressif.com/en/products/sdks/esp-idf) version 3.x (4.x support is currently _experimental_)
* `cmake`
* GNU `make`

Instructions for downloading and installing the Espressif IDF SDK and tool chains are outside of the scope of this document.  Please consult the [IDF SDKGetting Started](https://docs.espressif.com/projects/esp-idf/en/release-v3.3/get-started/index.html) guide for more information.

### Build Instructions

Change directories to the `src/platforms/esp32` directory under the AtomVM source tree root:

	shell$ cd <atomvm-source-tree-root>
	shell$ cd src/platforms/esp32

Start by updating the configuration of local `sdkconfig` file via the `menuconfig` Make target:

	shell$ make menuconfig

This command will bring up a curses dialog box.  You can exit the program by typing `E`.  Save the changes, and the program will exit.

You can now build AtomvVM using the `make` command:

	shell$ make -j 8
	...

> Note.  You may specify `-j <n>`, where `<n>` is the number of CPUs you would like to assign to run the build in parallel.

This command, once completed, will create the Espressif bootloader, partition table, and AtomVM binary.  The last line of the output should read something like the following:

	To flash all build output, run 'make flash' or:
	python /path/to/esp-idf-sdk/components/esptool_py/esptool/esptool.py --chip esp32
	--port /dev/ttyUSB0 --baud 115200 --before default_reset --after hard_reset write_flash
	-z --flash_mode dio --flash_freq 40m --flash_size detect
	0x1000 /path-to-atomvm-source-tree/Atomvm/src/platforms/esp32/build/bootloader/bootloader.bin
	0x10000 /path-to-atomvm-source-tree/Atomvm/src/platforms/esp32/build/atomvvm-esp32.bin
	0x8000 /path-to-atomvm-source-tree/Atomvm/src/platforms/esp32/build/partitions.bin

At this point, you can run `make flash` to upload the 3 binaries up to your ESP32 device, and in some development scenarios, this is a preferable shortcut.

However, first, we will build a single binary image file containing all of the above 3 binaries, as well as the AtomVM core libraries.  See [Building a Release Image](#building-a-release-image), below.  But first, it is helpful to understand a bit about how the AtomVM partitioning scheme works, on the ESP32.

### Flash Layout

The AtomVM Flash memory is partitioned to include areas for the above binary artifacts created from the build, as well areas for runtime information used by the ESP32 and compiled Erlang/Elixire code.

The flash layout is roughly as follows (not to scale):

    +-----------------+  ------------- 0x0000
    |    secure       |
    |     boot        | 4KB
    |                 |
    +-----------------+  ------------- 0x1000
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
    |     lib.avm     | 256KB       v
    +-----------------+  ------------- 0x210000
    |                 |             ^
    |                 |             |
    |     main.avm    | 1MB+        | Erlang/Elixir
    |                 |             | Application
    |                 |             |
    |                 |             v
    +-----------------+  ------------- end

The following table summarizes the partitions created on the ESP32 when deploying AtomVM:

| Partition | Offset | Length | Description |
|:----------|:-------|:-------|:------------|
| Secure Boot | 0x00 | 4kB | Initialization vectors and other data needed for ESP32 secure boot. |
| Bootloader | 0x1000 | 28kB | The ESP32 bootloader, as built from the IDF-SDK.  AtomVM does not define its own bootloader. |
| Partition Table | 0x8000 | 3kB | The AtomVM-defined partition table. |
| NVS | 0x9000 | 24kB | Space for non-volatile storage. |
| PHY_INIT | 0xF000 | 4kB | Initialization data for physical layer radio signal data. |
| AtomVM virtual machine | 0x10000 | 1.75mB | The AtomVM virtual machine (compiled from C code). |
| lib.avm | 0x1D0000 | 256k | The AtomVM BEAM library, compiled from Erlang and Elixir files in the AtomVM source tree. |
| main.avm | 0x210000 | 1mB | The user application.  This is where users flash their compiled Erlang/Elixir code |

### The `lib.avm` and `main.avm` partitions

The `lib.avm` and `main.avm` partitions are intended to store Erlang/Elixir libraries (compiled down to BEAM files, and assembled as AVM files).

The `lib.avm` partition is intended for core Erlang/Elixir libraries that are built as part of the AtomVM build.  The release image of AtomVM (see below) includes both the AtomVM virtual machine and the `lib.avm` partition, which includes the BEAM files from the `estdlib` and `eavmlib` libraries.

In contrast, the `main.avm` partition is intended for user applications.  Currently, the `main.avm` partition starts at address `0x210000`, and it is to that location to which application developers should flash their application AVM files.

The AtomVM search path for BEAM modules starts in the `main.avm` partition and falls back to `lib.avm`.  Users should not have a need to override any functionality in the `lib.avm` partition, but if necessary, a BEAM module of the same name in the `main.avm` partition will be loaded instead of the version in the `lib.avm` partition.

> Note.  The location of the `main.avm` partition may change over time, depending on the relative sizes of the AtomVM binary and `lib.avm` partitions.

### Building a Release Image

The `<atomvm-source-tree-root>/tools/release/esp32` directory contains the `mkimage.sh` script that can be used to create a single AtomVM image file, which can be distributed as a release, allowing application developers to develop AtomVM applications without having to build AtomVM from scratch.

> Note.  Before running the `mkimage.sh` script, you must have a complete build of both the esp32 project, as well as a full build of the core Erlang libraries in the `libs` directory.  The script configuration defaults to assuming that the core Erlang libraries will be written to the `build/libs` directory in the AtomVM source tree.  You should pass the `--build_dir <path>` option to the `mkimage.sh` script, with `<path>` pointing to your AtomVM build directory, if you  target a different build directory when running CMake.

Running this script will generate a single `atomvm-<sha>.img` file in the `build` directory of the esp32 source tree, where `<sha>` is the git hash of the current checkout.  This image contains the ESP32 bootloader, AtomVM executable, and the `eavmlib` and `estdlib` Erlang libraries in one file, which can then be flashed to address `0x1000`.

The `mkimage.sh` script is run as follows:

    shell$ ./tools/release/esp32/mkimage.sh
    Writing output to /home/frege/AtomVM/src/platforms/esp32/build/atomvm-602e6bc.img
    =============================================
    Wrote bootloader at offset 0x1000 (4096)
    Wrote partition-table at offset 0x8000 (32768)
    Wrote AtomvVM Virtual Machine at offset 0x10000 (65536)
    Wrote AtomvVM Core BEAM Library at offset 0x110000 (1114112)

Users can then use the `esptool.py` directly to flash the entire image to the ESP32 device, and then flash their applications to the `main.app` partition at address `0x210000`,

But first, it is a good idea to erase the flash, e.g.,

    shell$ esptool.py --chip esp32 --port /dev/ttyUSB0 erase_flash
    esptool.py v2.1
    Connecting........_
    Chip is ESP32D0WDQ6 (revision 1)
    Uploading stub...
    Running stub...
    Stub running...
    Erasing flash (this may take a while)...
    Chip erase completed successfully in 5.4s
    Hard resetting...

You can then use the `./tools/dev/flash.sh` tool (or `esptool.py` directly, if you prefer), to flash the entire image to your device:

    shell$ FLASH_OFFSET=0x1000 ./tools/dev/flash.sh ./src/platforms/esp32/build/atomvm-602e6bc.img
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

> Note. Flashing the full AtomVM image will delete all entries in non-volatile storage.  Only flash the full image if you have a way to recover and re-write any such data.

Finally, you can then flash your own application, e.g.,

    shell$ ./tools/dev/flash.sh examples/erlang/esp32/blink.avm
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

> Note.  Since the Erlang core libraries are flashed to the ESP32 device, it is not necessary to include core libraries in your application AVM files.  Users may be interested in using downstream development tools, such as the Elixir <a href="https://github.com/atomvm/ExAtomVM">ExAtomVM Mix task</a>, or the Erlang <a href="https://github.com/fadushin/atomvm_rebar3_plugin">AtomVM Rebar3 Plugin</a> for doing day-to-day development of applications for the AtomVM platform.

### Adding custom Nifs, Ports, and third-party components

While AtomVM is a functional implementation of the Erlang virtual machine, it is nonetheless designed to allow developers to extend the VM to support additional integrations with peripherals and protocols that are not otherwise supported in the core virtual machine.

AtomVM supports extensions to the VM via the implementation of custom native functions (Nifs) and processes (AtomVM Ports), allowing users to extend the VM for additional functionality, and you can add your own custom Nifs, ports, and additional third-party components to your ESP32 build by adding them to the `components` directory, and the ESP32 build will compile them automatically.

> For more information about building components for the IDF SDK, consult the [IDF SDK Build System](https://docs.espressif.com/projects/esp-idf/en/v3.3.2/api-guides/build-system.html#) documentation.

The instructions for adding custom Nifs and ports differ in slight detail, but are otherwise quite similar.  In general, they involve:

1. Adding the custom Nif or Port to the `comonents` directory of the AtomVM source tree;
1. Adding the component to the corresponding `main/component_nifs.txt` or `main/component_ports.txt` files;
1. Building the AtomVM binary.

> Note.  The Espressif SDK and tool chains do not, unfortunately, support dynamic loading of shared libraries and dynamic symbol lookup.  In fact, dynamic libraries are not supported at all on the ESP32 using the IDF SDK; instead, any code that is needed at runtime must be statically linked into the application.

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

    void my_nif_init(GlobalContext *global);
    const struct Nif *my_nif_get_nif(const char *nifname);

> Note. Instructions for implementing Nifs is outside of the scope of this document.

* Add your `<moniker>` to the `main/component_nifs.txt` file in the `src/platforms/esp32` directory.

> Note.  The `main/component_nifs.txt` file will not exist until after the first clean build.

#### Adding a custom AtomVM Port

To add support for a new peripheral or protocol using an AtomVM port, you need to do the following:

* Choose a name for your port (e.g, "my_port").  Call this `<moniker>`.
* In your source code, implement the following two functions:
    * `void <moniker>_init(GlobalContext *global);`
        * This function will be called once, when the application is started.
    * `Context *<moniker>_create_port(GlobalContext *global, term opts);`
        * This function will be called to locate the Nif during a function call.

Example:

    void my_port_init(GlobalContext *global);
    Context *my_port_create_port(GlobalContext *global, term opts);

> Note. Instructions for implementing Ports is outside of the scope of this document.

* Add your `<moniker>` to the `main/component_ports.txt` file in the `src/platforms/esp32` directory.

> Note.  The `main/component_ports.txt` file will not exist until after the first clean build.

## Building for STM32

> TODO
