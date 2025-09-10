<!--
 Copyright 2021-2022 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Getting Started Guide

Welcome to the AtomVM Getting Started Guide.  This document is intended to get you started so that you can run Erlang or Elixir programs on the AtomVM platform as quickly as possible.

In order to do so, you will need to provision your device (depending on the device type) with the AtomVM virtual machine.  Typically, you only need to do this once (or at least once per release of the VM you would like to use).  Once the VM is provisioned on the device, you can then deploy your application onto the device, and we expect this process to your typical "deploy, test, debug" development lifecycle.  The subsequent chapter on [AtomVM Tooling](./atomvm-tooling.md) will help you understand that process.

The getting started is broken up into the following sections:

* [Getting Started on the ESP32 platform](#getting-started-on-the-esp32-platform)
* [Getting Started on the STM32 platform](#getting-started-on-the-stm32-platform)
* [Getting Started on the Raspberry Pi RP2](#getting-started-on-the-raspberry-pi-rp2)
* [Getting Started on the Generic UNIX platform](#getting-started-on-the-generic-unix-platform)
* [Getting Started with AtomVM WebAssembly](#getting-started-with-atomvm-webassembly)

Please use the appropriate section for the device type you intend to use.

## Getting Started on the ESP32 platform

The AtomVM virtual machine is supported on the [Espressif](https://www.espressif.com) [ESP32](https://www.espressif.com/en/products/socs/esp32) platform, allowing users to write Erlang and Elixir programs and deploy them to the ESP32 micro-controller.  For specific information about which ESP32 boards and chip-sets are supported, please refer to the AtomVM [Release Notes](./release-notes.md).

These instructions cover how to provision the AtomVM virtual machine flashed to your ESP32 device.

For most applications, you should only need to install the VM once (or at least once per desired AtomVM release).  Once the VM is uploaded, you can then begin development of Erlang or Elixir applications, which can then be flashed as part of your routine development cycle.

### ESP32 Requirements

Deployment of AtomVM on the ESP32 platform requires the following components:

* A computer running MacOS or Linux (Windows support is not currently supported);
* An ESP32 (including ESP32-S2, ESP32-S3, ESP32-C2, ESP32-C3, ESP32-C6, ESP32-H2) module with a USB/UART connector (typically part of an ESP32 development board);
* A USB cable capable of connecting the ESP32 module or board to your development machine (laptop or PC);
* The [`esptool`](https://github.com/espressif/esptool) program, for flashing the AtomVM image and AtomVM programs;
* An [Erlang/OTP](https://erlang.org);
* A serial console program, such as `minicom` or `screen`, so that you can view console output from your AtomVM application.
* (recommended) For Erlang programs, [`rebar3`](https://rebar3.org);
* (recommended) For Elixir programs, [`mix`](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html), which ships with the Elixir runtime;

```{seealso}
For information about specific versions of required software, see the AtomVM [Release Notes](./release-notes.md).
```

### ESP32 Deployment Overview

The ES32 AtomVM virtual machine is an IDF application that runs on the ESP32 platform.  As an IDF application, it provides the object code to boot the ESP device and execute the AtomVM virtual machine code, which in turn is responsible for execution of an Erlang/Elixir application.

The AtomVM virtual machine is implemented in C, and the AtomVM binary image contains the binary object code compiled from C source files, as well as the ESP boot loader and partition map, which tells the ESP32 how the flash module is laid out.

AtomVM developers will typically write their applications in Erlang or Elixir.  These source files are compiled into BEAM bytecode, which is then assembled into AtomVM "packbeam" (`.avm`) files.  This packbeam file is flashed onto the ESP32 device, starting at the data partition address `0x210000`.  When AtomVM starts, it will look in this partition for the first occurrence of a BEAM module that exports a `start/0` function.  Once that module is located, execution of the BEAM bytecode will commence at that point.

The following diagram provides a simplified overview of the layout of the AtomVM virtual machine and Erlang/Elixir applications on the ESP32 flash module.

    |               |
    +---------------+  ----------- 0x0 | 0x1000 | 0x2000 (varies by esp32 flavor)
    | boot loader   |           ^
    +---------------+           |
    | partition map |           | AtomVM
    +---------------+           | binary
    |               |           | image
    |   AtomVM      |           |
    |   Virtual     |           |
    |   Machine     |           |
    |               |           v
    +---------------+  ----------- 0x210000 for thin images or
    |               |           ^  0x250000 for images with Elixir modules
    |               |           |
    |     data      |           | Erlang/Elixir
    |   partition   |           | Application
    |               |           |
    |               |           v
    +---------------+  ----------- end

Deploying an AtomVM application to an ESP32 device typically involved two steps:

1. Connecting the ESP32 device;
1. Deploying the AtomVM virtual machine;
1. Deploying an AtomVM application (typically an iterative process)

These steps are described in more detail below.

### Connecting the ESP32 device

Connect the ESP32 to your development machine (e.g., laptop or PC) via a USB cable.

    +---------------+
    | laptop or PC  |
    |               |           +-------+
    |               | USB       |       |
    |               x-----------x       |
    |               |           |       |
    |               |           +-------+
    +---------------+           ESP32

```{important}
There are a wide variety of ESP32 modules, ranging from home-made breadboard solutions to all-in-one development
boards.  For simplicity, we assume a development board that can both be powered by a USB cable and which can be
simultaneously flashed using the same cable, e.g., the
[Espressif ESP32 DevKit](https://www.espressif.com/en/products/devkits/esp32-devkitc).
```

Consult your local development board documentation for instructions about how to connect your device to your development machine.

### Deploying the ESP32 AtomVM virtual machine

The following methods can be used to deploy the AtomVM virtual machine to an ESP32 device:

1. Flashing a binary image;
1. Building from source.

#### Flashing a binary image to ESP32

Flashing the ESP32 using a pre-built binary image is by far the easiest path to getting started with development on the ESP32.  Binary images contain the virtual machine image and all of the necessary components to run your application.

We recommend first erasing any existing applications on the ESP32 device.  E.g.,

```shell
$ esptool.py --chip auto --port /dev/ttyUSB0 --baud 921600 erase_flash
```

```{note}
Specify the device port and baud settings and AtomVM image name to suit your particular environment.  A baud rate of 921600 works well for most ESP32 devices, some can work reliably at higher rates of 1500000, or even 2000000, but some devices (especially those with a 26Mhz crystal frequency, rather than the more common 40 Mhz crystal) may need to use a slower baud rate such as 115200.
```

Download the latest [release image](https://github.com/atomvm/AtomVM/releases) for ESP32.

This image will generally take the form:

>`Atomvm-<esp32-soc>-<atomvm-version>.img`

For example:

>`Atomvm-esp32-v0.6.0.img`

You will also find the sha256 hash for this file, which you should verify using the `sha256sum` command on your local operating system.

```{warning}
Alpha and Beta images may be unstable and may result in unpredictable behavior.  You can help solve these bugs by
opening a detailed [issue on GitHub](https://github.com/atomvm/AtomVM/issues), if you encounter such problems.
```

Finally, use the `esptool.py` command to flash the image to the bootloader start address `0x1000` on the ESP32.  E.g.,

```shell
$ esptool.py \
--chip auto \
--port /dev/ttyUSB0 --baud 921600 \
--before default_reset --after hard_reset \
write_flash -u \
--flash_mode dio --flash_freq 40m --flash_size detect \
0x1000 \
/path/to/Atomvm-esp32-v0.6.0.img
```

```{attention}
A baud rate of 921600 works well for most ESP32 devices, some can work reliably at higher rates of 1500000, or even
2000000, but some devices (especially those with a 26Mhz crystal frequency, rather than the more common 40 Mhz
crystal) may need to use a slower baud rate such as 115200.
```

The chart below lists the bootloader offset for the various ESP32 family of chips:

| Chipset | Bootloader offset |
|----------|----------|
| ESP32 | `0x1000` |
| ESP32-S2 | `0x1000` |
| ESP32-S3 | `0x0` |
| ESP32-C2 | `0x0` |
| ESP32-C3 | `0x0` |
| ESP32-C6 | `0x0` |
| ESP32-H2 | `0x0` |
| ESP32-P4 | `0x2000` |

Once completed, your ESP32 device is ready to run Erlang or Elixir programs targeted for AtomVM.

#### Building for ESP32 from source

You may optionally build AtomVM from source and deploy the AtomVM virtual machine to your ESP32 device manually.  Building AtomVM from source is slightly more involved, as it requires the installation of the Espressif IDF SDK and tool chain and is typically recommended only for users who are doing development on the AtomVM virtual machine, or for developers implementing custom Nifs or ports.

Instructions for building AtomVM from source are covered in the AtomVM [Build Instructions](./build-instructions.md#building-for-esp32)

### Deploying an AtomVM application for ESP32

An AtomVM application is a collection of BEAM files, which have been compiled using the Erlang or Elixir compiler.  These BEAM files are assembled into an AtomVM "packbeam" (`.avm`) file, which in turn is flashed to the `main` data partition on the ESP32 flash module, starting at address `0x210000` if you are using a thin image, or `0x250000` for images with Elixir support.

When the AtomVM virtual machine starts, it will search for the first module that contains an exported `start/0` function in this partition, and it will begin execution of the BEAM bytecode at that function.

AtomVM applications can be written in Erlang or Elixir, or a combination of both.  The AtomVM community has provided tooling for both platforms, making deployment of AtomVM applications as seamless as possible.

For information about how to flash your application to your ESP32, see the [AtomVM Tooling](./atomvm-tooling.md#esp32) chapter.

## Getting Started on the STM32 platform

AtomVM can run on a wide variety of STM32 chip-sets available from [STMicroelectronics](https://www.st.com). The support is not nearly as mature as for the ESP32 platform, but work is ongoing, and pull requests are always welcome. At this time AtomVM will work on any board with a minimum of around 128KB ram and 512KB (1M recommended) flash. Simple applications and tests have been successfully run on a stm32f411ceu6 (A.K.A. Black Pill V2). These minimum requirements may need to be raised as platform support matures.

### STM32 Requirements

Deployment of AtomVM on the STM32 platform requires the following components:

* A computer running MacOS or Linux (Windows is not currently supported);
* An stm32 board and a USB/UART connector (these are built into some boards such as the Nucleo product line) and a minimum of 512k (1M recommended) of flash and a recommended minimum of 100k RAM;
* A USB cable capable of connecting the STM32 module or board to your development machine (laptop or PC);
* `st-flash` via [stlink](https://github.com/stlink-org/stlink), to flash both AtomVM and your packed AVM applications. Make sure to follow its [installation procedure](https://github.com/stlink-org/stlink#installation) before proceeding further.
* A [st-link v2](https://www.st.com/en/development-tools/st-link-v2.html) or [st-link v3](https://www.st.com/en/development-tools/stlink-v3set.html) device (typically already included on Nucleo and Discovery boards), is needed for flashing and optional jtag debugging.
* A serial console program, such as `minicom` or `screen`, so that you can view console output from your AtomVM application.
* (recommended) For Erlang programs, [`rebar3`](https://rebar3.org);
* (recommended) For Elixir programs, [`mix`](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html), which ships with the Elixir runtime;

### Deploying the STM32 AtomVM virtual machine

The following methods can be used to deploy the AtomVM virtual machine to an STM32 device:

1. Building from source.

```{attention}
Due to the very large number of supported chip-sets and the wide variety of board configurations, and the code
changes required to support them, pre-built binaries for the stm32 platform are not currently available.
```

Consult the [STM32 Build Instructions](./build-instructions.md#building-for-stm32) to create a binary compatible with your board.

#### Flashing a binary image to STM32

Once you have created an STM32 binary image, you can flash the image to your STM32 device using the `st-flash` application.

To flash your image, use the following command:

```shell
$ st-flash --reset write AtomVM-stm32f407vgt6.bin 0x8000000
```

Congratulations!  You have now flashed the AtomVM VM image onto your STM32 device!

```{important}
AtomVM expects to find the AVM at the address 0x8080000. On a STM32 Discovery board this means that the 1MB of flash
will be split in 512KB available for the program and 512KB available for the packed AVM. For devices with only 512KB
of flash the application address is 0x8060000, leaving 128KB of application flash available.
```

#### Console Printing

By default, stdout and stderr are printed on USART2. On the STM32F4Discovery board, you can see them using a TTL-USB with the TX pin connected to board's pin PA2 (USART2 RX). Baudrate is 115200 and serial transmission is 8N1 with no flow control.

For Nucleo boards the on board USB-COM to USART may be used by configuring your build with a BOARD parameter, see the [STM32 Build Instructions](./build-instructions.md#building-for-stm32) for [Configuring the Console](./build-instructions.md#configuring-the-console).

### Deploying an AtomVM application for STM32

An AtomVM application is a collection of BEAM files, which have been compiled using the Erlang or Elixir compiler.  These BEAM files are assembled into an AtomVM "packbeam" (`.avm`) file, which in turn is flashed to the `main` data partition on the STM32 flash module, starting at address `0x8080000`, for boards with 512KB of flash the address is `0x8060000`.

When the AtomVM virtual machine starts, it will search for the first module that contains an exported `start/0` function in this partition, and it will begin execution of the BEAM bytecode at that function.

AtomVM applications can be written in Erlang or Elixir, or a combination of both.  The AtomVM community has provided tooling for both platforms, making deployment of AtomVM applications as seamless as possible.

For information about how to flash your application to your STM32, see the [AtomVM Tooling](./atomvm-tooling.md) chapter.

## Getting Started on the Raspberry Pi RP2

AtomVM supports deployment of the VM and applications onto boards based on Raspberry Pi RP2 socs, including RP2040 and RP2350. This includes [Raspberry Pi Pico and Pico W](https://www.raspberrypi.com/products/raspberry-pi-pico/), as well as [Raspberry Pi Pico 2 and Pico 2 W](https://www.raspberrypi.com/products/raspberry-pi-pico-2/) boards.  For information about supported boards, please refer to the AtomVM [Release Notes](./release-notes.md).

The following instructions show you how to install the AtomVM onto one of the [Raspberry Pi Pico](https://www.raspberrypi.com/products/raspberry-pi-pico/) boards.

### Pico and Pico 2 Requirements

Deployment of AtomVM on the Raspberry Pi Pico and Pico 2 platform requires the following components:

* A computer running MacOS or Linux (Windows support is not currently supported);
* A Raspberry Pi Pico or Pico 2 board;
* A USB cable capable of connecting the board to your development machine (laptop or PC);
* A serial console program, such as `minicom` or `screen`, so that you can view console output from your AtomVM application.
* (recommended) For Erlang programs, [`rebar3`](https://rebar3.org);
* (recommended) For Elixir programs, [`mix`](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html), which ships with the Elixir runtime;
* (recommended) [`picotool`](https://github.com/raspberrypi/picotool), useful for resetting the device into `BOOTSEL` or `application` mode, optionally used by the `atomvm_rebar3_plugin` (if available in env $PATH) for disconnecting active `screen` sessions when attempting to flash when still connected.

### Deploying the Pico or Pico 2 AtomVM virtual machine

The following methods can be used to deploy the AtomVM virtual machine to a Raspberry Pico device:

1. Flashing a binary image;
1. Building from source.

#### Flashing a binary image to Pico

Flashing the Raspberry Pi Pico or Pico 2 using a pre-built binary image is by far the easiest path to getting started with development on this platform.  Binary images contain the virtual machine image and all of the necessary components to run your application.

Download the latest [release image](https://github.com/atomvm/AtomVM/releases) for Raspberry Pi Pico or Raspberry Pi Pico 2.

This image will generally take the form:

>`Atomvm-<raspberry-pico-board>-<atomvm-version>.uf2`

For example:

>`Atomvm-pico-v0.7.0.uf2`

or

>`Atomvm-pico2-v0.7.0.uf2`

You will also find the sha256 hash for this file, which you should verify using the `sha256sum` command on your local operating system.

You will also need a copy of the AtomVM core libraries, which include all of the compiled Erlang and Elixir needed to run parts of the VM.

This library will generally take the form:

>`atomvmlib-<raspberry-pico-board>-<atomvm-version>.uf2`

For example:

>`atomvmlib-pico-v0.7.0.uf2`

You will also find the sha256 hash for this file, which you should verify using the `sha256sum` command on your local operating system.

To flash your Raspberry Pico, you will need to undertake a few steps that interact with your operating file system.

```{important}
It is important that you downloads the `.uf2` versions of these files for the Raspberry Pi Pico or Pico 2 platform. It is also important to download the Pico or the Pico2 files depending on your board.
```

For each of the above files, you will start your Raspberry Pi Pico or Pico 2 in bootloader mode by pressing the `BOOTSEL` button on the board, while powering on the device.  Doing so will automatically boot the device and mount the Raspberry Pi Pico on to your file system as a USB device.

You can then use normal operating system commands (such as `cp`, or even drag-and-drop) to copy the above files to the mounted USB volume.

Note, however, that in general the USB device will auto-unmount after each file has been copied, so you will need to repeat the procedure for each of the above two files.

On most Linux systems, the Raspberry Pi Pico will be mounted at `/run/media/${USER}/RPI-RP2` and the Raspberry Pi Pico 2 will be mounted at `/run/media/${USER}/RP2350`.

On macOS system, the Raspberry Pi Pico will be mounted at `/Volumes/RPI-RP2` and the Raspberry Pi Pico 2 will be mounted at `/Volumes/RP2350`.

For example:

>Power on Raspberry Pico with BOOTSEL button pressed

```shell
$ ls -l /Volumes/RPI-RP2
total 16
-rwxrwxrwx  1 joe  staff  241 Sep  5  2008 INDEX.HTM*
-rwxrwxrwx  1 joe  staff   62 Sep  5  2008 INFO_UF2.TXT*

$ cp ~/Downloads/AtomVM-pico-v0.7.0.uf2 /Volumes/RPI-RP2/.
```

... at this point, the device will auto-unmount.

And again for the AtomVM core library (note that previously flashed `.uf2` files have disappeared):

>Power on Raspberry Pico with BOOTSEL button pressed

```shell
$ ls -l /Volumes/RPI-RP2
total 16
-rwxrwxrwx  1 joe  staff  241 Sep  5  2008 INDEX.HTM*
-rwxrwxrwx  1 joe  staff   62 Sep  5  2008 INFO_UF2.TXT*

$ cp ~/Downloads/atomvmlib-pico-v0.7.0.uf2 /Volumes/RPI-RP2/.
```

... and again, at this point, the device will auto-unmount.

### Potential Issues with macOS

There are known issues copying files to the Pico using macOS, and a lot of literature online. Usually it's best to use the Terminal rather than the Finder because the errors are more explicit.
Copying may also fail with UF2 files downloaded from the Internet, typically AtomVM release binaries.

```shell
$ cp ~/Downloads/AtomVM-pico_w-v0.6.0.uf2 /Volumes/RPI-RP2/.
cp: /Volumes/RPI-RP2/AtomVM-pico-v0.6.0.uf2: fcopyfile failed: Operation not permitted
cp: /Users/joe/Downloads/AtomVM-pico-v0.6.0.uf2: could not copy extended attributes to 
/Volumes/RPI-RP2/AtomVM-pico-v0.6.0.uf2: Operation not permitted
```

Two issues appear here: one is macOS tries to copy extended attributes and this fails (but this error is not a blocker), and the other is the "Operation not permitted" because the file is quarantined, having been downloaded from the web.

First issue can be solved with `cp -x` if you don't tolerate the error message and second with `xattr -d`.

```shell
$ xattr -d com.apple.quarantine ~/Downloads/AtomVM-pico_w-v0.6.0.uf2
$ cp -x ~/Downloads/AtomVM-pico_w-v0.6.0.uf2 /Volumes/RPI-RP2/.
$
```

### Deploying an AtomVM application for Generic Unix

An AtomVM application is a collection of BEAM files, which have been compiled using the Erlang or Elixir compiler.  These BEAM files are assembled into an AtomVM "packbeam" (`.avm`) file, which in turn can be provided to the `atomvm` executable on the command line.

When the AtomVM virtual machine starts, it will search for the first module that contains an exported `start/0` function in this partition, and it will begin execution of the BEAM bytecode at that function.

AtomVM applications can be written in Erlang or Elixir, or a combination of both.  The AtomVM community has provided tooling for both platforms, making deployment of AtomVM applications as seamless as possible.

For information about how to flash your application to your Raspberry Pi Pico, see the [AtomVM Tooling](./atomvm-tooling.md#raspberry-pico) chapter.

## Getting Started on the Generic UNIX platform

The AtomVM virtual machine is supported a wide variety of Generic UNIX platforms, including many Linux kernels and target architectures, FreeBSD, and MacOS, allowing users to write Erlang and Elixir programs and run them on a local development machine.  For specific information about which Generic UNIX versions and architectures are supported, please refer to the AtomVM [Release Notes](./release-notes.md).

These instructions cover how to provision the AtomVM virtual machine onto your development machine.  Running applications locally can sometimes be a useful exercise in debugging.

```{caution}
Not all programming interfaces are supported on all platforms.  See the AtomVM
[Programmers Guide](./programmers-guide.md) for more information.
```

For most applications, you should only need to install the VM once (or at least once per desired AtomVM release).  Once the VM is installed, you can then begin development of Erlang or Elixir applications, which can then be flashed as part of your routine development cycle.

### Generic UNIX Requirements

Deployment of AtomVM on the Generic UNIX platform requires the following components:

* A computer running MacOS or Linux (Windows support is not currently supported);
* An [Erlang/OTP](https://erlang.org) and compatible [Elixir](https://elixir-lang.org) runtime;
* (recommended) For Erlang programs, [`rebar3`](https://rebar3.org);
* (recommended) For Elixir programs, [`mix`](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html), which ships with the Elixir runtime;

For information about specific versions of required software, see the AtomVM [Release Notes](./release-notes.md).

### Installing the AtomVM virtual machine

The following methods can be used to install the AtomVM virtual machine on the Generic UNIX platform:

1. Download Linux Binaries
1. (MacOS only) Installing via [macports](https://www.macports.org) or [Homebrew](https://brew.sh);
1. Building from source.

#### Installation on Linux Platforms

Downloading a pre-built binary image for Linux is by far the easiest path to getting started with development on a Linux development machine.  Binary images contain the virtual machine.

Download the latest [release image](https://github.com/atomvm/AtomVM/releases) for Linux.

This image will generally take the form:

>`Atomvm-linux-<arch>-<atomvm-version>`

where `<arch>` is the target architecture.

For example:

>`Atomvm-linux-x86_64-v0.6.0`

You will also find the sha256 hash for this file, which you should verify using the `sha256sum` command on your local operating system.

You will also need a copy of the AtomVM core libraries, which include all of the compiled Erlang and Elixir needed to run parts of the VM.

This library will generally take the form:

>`atomvmlib-<atomvm-version>.avm`

For example:

>`atomvmlib-v0.6.0.avm`

You will also find the sha256 hash for this file, which you should verify using the `sha256sum` command on your local operating system.

```{seealso}
[See below](#running-applications-on-the-generic-unix-platform) for instructions about how to run the `AtomVM`
binary, together with the AtomVM core libraries on the command line.
```

#### Installation on MacOS

You can install AtomVM for Generic UNIX using [macports](https://www.macports.org) or [Homebrew](https://brew.sh).  This instructions assume you are familiar with these package managers.

To install via [macports](https://www.macports.org):

```shell
$ sudo port install atomvm
```

Once installed, the `atomvm` executable should be available in your `$PATH` environment variable.

```shell
$ which atomvm
/opt/local/bin/atomvm
```

To install via [Homebrew](https://brew.sh), you will first need to install the `atomvm` Homebrew Tap:

```shell
$ brew tap atomvm/atomvm
```

This command will make the `atomvm` [Homebrew](https://brew.sh) formula available to you.

```shell
$ brew install atomvm
```

Once installed, the `atomvm` executable should be available in your `$PATH` environment variable.

```shell
$ which atomvm
/usr/local/bin/atomvm
```

#### Building on MacOS from source

You may optionally build AtomVM from source and install the AtomVM virtual machine to your development machine.  Building AtomVM from source is slightly more involved, as it requires the installation of third party libraries and is typically recommended only for users who are doing development on the AtomVM virtual machine, or for developers implementing custom Nifs or ports.

Instructions for building AtomVM from source are covered in the AtomVM [Build Instructions](./build-instructions.md#generic-unix-build-instructions).

### Running applications on the Generic UNIX platform

AtomVM may be run on UNIX-like platforms using the `atomvm` command.

You may specify one or more AVM files on the command line when running the `atomvm` command.  BEAM modules defined in earlier AVM modules on the command line take higher precedence that BEAM modules included in AVM files later in the argument list.

```shell
$ atomvm /path/to/myapp.avm
```

To get the current version of AtomVM, use the `-v` option, e.g.:

```shell
$ atomvm -v
0.6.0
```

Use the `-h` option to get command line help:

```shell
$ atomvm -h

Syntax:

    /usr/local/lib/atomvm/AtomVM [-h] [-v] <path-to-avm-file>+

Options:

    -h         Print this help and exit.
    -v         Print the AtomVM version and exit.

Supply one or more AtomVM packbeam (.avm) files to start your application.

Example:

    $ /usr/local/lib/atomvm/AtomVM /path/to/my/application.avm /path/to/atomvmlib.avm
```

## Getting Started with AtomVM WebAssembly

You can run AtomVM for WebAssembly with NodeJS or within common browsers (Safari, Chrome and Chrome-based, Firefox).

### Getting Started with AtomVM WebAssembly port for NodeJS

Download the latest [release image](https://github.com/atomvm/AtomVM/releases) for Node.

This image will generally take the form:

>`Atomvm-node-<atomvm-version>.js`

For example:

>`Atomvm-node-v0.6.0.js`

You will also find the sha256 hash for this file, which you should verify using the `sha256sum` command on your local operating system.

AtomVM's WebAssembly port for NodeJS may be run using `node` command and AtomVM.js, AtomVM.worker.js and AtomVM.wasm files.

```shell
$ node /path/to/Atomvm-node-v0.6.0.js /path/to/myapp.avm
```

### Getting Started with AtomVM WebAssembly port for browsers

AtomVM may also be run in modern browsers (Safari, Chrome and Chrome-based, Firefox) using AtomVM.js, AtomVM.worker.js and AtomVM.wasm files.

Please note that these files are different from the NodeJS ones.

Because AtomVM uses SharedArrayBuffer, to be executed by a browser, these files need to be served:

* on localhost or over HTTPS
* by a web server that also sends `Cross-Origin-Opener-Policy` and `Cross-Origin-Embedder-Policy` headers. These headers are also called COOP and COEP headers.

These security requirements are documented in [Mozilla's documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer#security_requirements).

#### Trying locally from AtomVM source tree

If you compile AtomVM for Unix as well as for Node as explained in the [build instructions](./build-instructions.md#building-for-emscripten), you can use an AtomVM-based toy webserver to serve the WebAssembly examples with:

```shell
$ ./src/AtomVM examples/emscripten/wasm_webserver.avm
```

This web server serves HTML files from `examples/emscripten/`. It works without HTTPS because files are served on localhost.

#### Using a hosting service with a `_headers` file

You can also host the three files on a hosting service such as Netlify that uses `_headers` files.

The file could have the following content:

```cfg
/*
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

#### Using web server such as Nginx

You can also host the three files on web server such as Nginx or Apache.

The configuration for Nginx would be:

```cfg
server {
    add_header Cross-Origin-Opener-Policy "same-origin";
    add_header Cross-Origin-Embedder-Policy "require-corp";
    location / {
        ...
    }
}
```

#### Using Javascript service worker trick

If you have no possibility to modify the headers, for example with GitHub pages, you can still get AtomVM to run in the browser using a Javascript service worker trick.

We did successfully use [coi-serviceworker](https://github.com/gzuidhof/coi-serviceworker).

## Where to go from here

The following resources may be useful for understanding how to develop Erlang or Elixir applications for the AtomVM platform:

* [AtomVM Tooling](./atomvm-tooling.md)
* [Example Programs](https://github.com/atomvm/atomvm_examples)
* [Programmers Guide](./programmers-guide.md)
