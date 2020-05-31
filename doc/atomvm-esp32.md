# AtomVM on the ESP32

These notes describe special considerations about the implementation of AtomVM on the <a href="https://www.espressif.com/en/products/socs/esp32/overview">ESP32</a> platform.

The ESP32 port of AtomVM makes use of the ESP <a href="https://docs.espressif.com/projects/esp-idf/en/v3.3.2/index.html">IDF-SDK</a>.

## Source Code

Platform-specific code for the ESP32 platform can be found under <a href="https://github.com/bettio/AtomVM/tree/master/src/platforms/esp32">`src/platforms/esp32`</a> in the AtomVM source tree.

The ESP32-specific parts of the source code include implementations of:

* the `main` entrypoint into the AtomVM program;
* system-specific API calls, such as integration points with the FreeRTOS and IDF-SDK;
* drivers for ESP32-specific functionality, such as GPIO, networking, non-volatile storage, etc;
* partitioning schemes for layout of code on ESP32 flash memory;
* integration with the IDF-SDK build system.

## AtomVM Build

AtomVM currently builds against the version 3 branch of the ESP IDF-SDK, and makes use of the make build system (as opposed to the CMake-based build, which is encouraged in version 4 and following of the IDF-SDK.)

> Note.  Build and setup instructions for the IDF SDK and Xtensa tool chain are outside of the scope of this document.

The build results in the production of 3 binary files, suitable deployment to an ESP32 device:

* `bootloader.bin` The ESP32 bootloader (as built from the IDF-SDK)
* `partitions.bin` A partition table, described below
* `atomvvm-esp32.bin` The actual Erlang/Elixier virtual machine

Once built, these binaries are "flashed" (i.e., uploaded, typically over the serial port) to the flash memory of the ESP32 device.

> Note that the AtomVM build will also build all of the "components" from the IDF SDK, which can sometimes take additional time to build on an initial run.  However, the Xtensa toolchain linker will only add code and symbols that are explicitly called from the AtomVM codebase, so the executable image size is typically less than the sum of the components that are compiled as part of the build.

## Flash Layout

The AtomVM Flash memory is partitioned to include areas for the above binary artifacts created from the build, as well areas for runtime information used by the ESP32 and compiled Erlang/Elixire code.

The flash layout is roughly as follows:

    0x00       0x1000           0x8000  0x9000         0xF000     0x10000                0x110000               0x210000
    +----------+-------- -------+-------+------- ------+----------+---------  -----------+----------  ----------+----------  ----------+
    |          |                |       |              |          |                      |                      |                      |
    +----------+-------- -------+-------+------- ------+----------+---------  -----------+----------  ----------+----------  ----------+
    |<-------->|<------- ------>|<----->|<------ ----->|<-------->|<--------  ---------->|<---------  --------->|<---------  --------->|
        4kB           28kB         3kB         24kB         4kB             1mB                     1mB                    1mB
      Secure      Bootloader    Partition      NVS       PHY_INIT          AtomVM                  lib.avm               main.avm
       Boot                       Table                                virtual machine

The following table summarizes the partitions created on the ESP32 when deploying AtomVM:

| Partition | Offset | Length | Description |
|:----------|:-------|:-------|:------------|
| Secure Boot | 0x00 | 4kB | Initialization vectors and other data needed for ESP32 secure boot. |
| Bootloader | 0x1000 | 28kB | The ESP32 bootloader, as built from the IDF-SDK.  AtomVM does not define its own bootloader. |
| Partition Table | 0x8000 | 3kB | The AtomVM-defined partition table. |
| NVS | 0x9000 | 24kB | Space for non-volatile storage. |
| PHY_INIT | 0xF000 | 4kB | Initialization data for physical layer radio signal data. |
| AtomVM virtual machine | 0x10000 | 1mB | The AtomVM virtual machine (compiled from C code). |
| lib.avm | 0x110000 | 1mB | The AtomVM BEAM library, compiled from Erlang and Elixir files in the AtomVM source tree. |
| main.avm | 0x210000 | 1mB | The user application.  This is where users flash their compiled Erlang/Elixir code |

### The `lib.avm` and `main.avm` partitions

The `lib.avm` and `main.avm` partitions are intended to store Erlang/Elixir libraries (compiled down to BEAM files, and assembled as AVM files).

The `lib.avm` partition is intended for core Erlang/Elixir libraries that are built as part of the AtomVM build.  The release image of AtomVM (see below) includes both the AtomVM virtual machine and the `lib.avm` partition, which includes the BEAM files from the `estdlib` and `eavmlib` libraries.

In contrast, the `main.avm` partition is intended for user applications.  Currently, the `main.avm` partition starts at address `0x110000`, and it is to that location to which application developers should flash their application AVM files.

The AtomVM search path for BEAM modules starts in the `main.avm` partition and falls back to `lib.avm`.  Users should not have a need to override any functionality in the `lib.avm` partition, but if necessary, a BEAM module of the same name in the `main.avm` partition will be loaded instead of the version in the `lib.avm` partition.

> Note.  The location of the `main.avm` partition may change over time, depending on the relative sizes of the AtomVM binary and `lib.avm` partitions.

## Networking

The ESP32 supports WIFI networking on both Station (STA) mode and Access Point (AP) modes, meaning that the ESP32 can connect to an existing access point, or can function as an access point, respectively.

> Note.  The AtomVM network implementation currently supports STA mode only.

The AtomVM API provides interfaces for initializing the WIFI network and is described in more detail in the `atomvm-network_fsm` documentation.

Credentials for connecting to a an access point in STA mode (SSID and password) can be specified programatically.  Alternatively, they may be set in non-volatile storage once, and then used for subsequent messages.

> Note.  The ESP32 also supports Bluetooth 4.0 networking.; however, AtomVM does not currently support Bluetooth networking.

AtomVM also supports UDP and TCP/IP networking and provides access to these protocols via the standard Erlang/OTP `gen_udp` and `gen_tcp` interfaces.  These interfaces are a part of the `estdlib` library and are installed in the `lib.avm` section of ESP32 flash memory.

> Note.  AtomVM does not yet support TLS communication over TCP/IP.

## Non-Volatile storage

The ESP32 supports create, read, update, and delete (CRUD) operations on entries in non-volatile storage.  Entries in non-volatile storage survive ESP32 device restarts, so can be useful for storing configuration information that may change at run-time.  Note, however, that Flash storage on ESP32 devices is sensitive to write operations, and may degrade with a large number of writes.

Non-colatile storage is a simple key-value store, with support for mutiple data types under a string key.  Keys can be namespaced with an additional namespace parameter.

AtomVM provides an API for creating, reading, updating, and deleting entries, but limits the namespace and key types to Erlang atoms and the value types to Erlang binaries.  Using `term_to_binary/1` and `binary_to_term/1`, applications can store virtually any data type in non-volatile storage.

AtomVM reserves the namesapce `atomvm` for its own internal uses.  Users should not create or manipulate entries in this namespace, unless explicitly instructed to do so (e.g., to configure the default SSID and password for the AtomVM networking APIs)

## Socket Driver

> TODO

## GPIO Driver

> TODO (currently under revision)

## UART Driver

> TODO

## SPI Driver

> TODO

## I2C Driver

> TODO

## Release Image

The AtomVM build will generate artifacts suitable for flashing to an ESP32 device, but generation of these artifacts requires building AtomVM from sources, which in turn requires the IDF SDK, the Xtensa tool chain, and so forth.

The `tools/release/esp32` directory contains the `mkimage.sh` script that can be used to create a single AtomVM image file, which can be distributed as a release, allowing application developers to develop AtomVM applications without having to build AtomVM from scratch.

Running this script will generate a single `atomvm-<sha>.img` file, where `<sha>` is the git hash of the current checkout.  This contains the ESP32 bootloader, AtomVM executable, and the `eavmlib` and `estdlib` Erlang libraries in one file, which should then be flashed to address `0x1000`.

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

    shell$ esptool.py  --chip esp32 --port /dev/ttyUSB0 erase_flash
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
