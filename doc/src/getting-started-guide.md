<!--
 Copyright 2021-2022 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Getting Started Guide

The getting started is broken up into the following sections:

* Getting Started on the ESP32 platform
* Getting Started on the STM32 platform
* Getting Started on the Generic UNIX platform

## Getting Started on the ESP32 platform

The AtomVM virtual machine is supported on the [Espressif](https://www.espressif.com) [ESP32](https://www.espressif.com/en/products/socs/esp32) platform, allowing users to write Erlang and Elixir programs and deploy them to the ESP32 micro-controller.

These instructions cover how to get the AtomVM virtual machine flashed to your ESP32 device, as well as how to flash your Erlang and Elixir programs that will be executed by the virtual machine running on the device.

For most applications, you should only need to install the VM once (or at least once per desired AtomVM release).  Once the VM is uploaded, you can then begin development of Erlang or Elixir applications, which can then be flashed as part of your routine development cycle.

### Requirements

Deployment of AtomVM applications requires the following components:

* A computer running MacOS or Linux (Windows support is not currently supported);
* An ESP32 module with a USB/UART connector (typically part of an ESP32 development board);
* A USB cable capable of connecting the ESP32 module or board to your development machine (laptop or PC);
* The [`esptool`](https://github.com/espressif/esptool) program, for flashing the AtomVM image and AtomVM programs;
* An [Erlang/OTP](https://erlang.org) release (21, 22, or 23);
* A serial console program, such as `minicom` or `screen`, so that you can view console output from your AtomVM application.
* (optional) For Erlang programs, [`rebar3`](https://rebar3.org);
* (optional) For Elixir programs, [`mix`](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html), which ships with the Elixir runtime;

### Deployment Overview

The ES32 AtomVM virtual machine is an IDF application that runs on the ESP32 platform.  As an IDF application, it provides the object code to boot the ESP device and execute the AtomVM virtual machine code, which in turn is responsible for execution of an Erlang/Elixir application.

The AtomVM virtual machine is implemented in C, and the AtomVM binary image contains the binary object code compiled from C source files, as well as the ESP boot loader and partition map, which tells the ESP32 how the flash module is layed out.

AtomVM developers will typically write their applications in Erlang or Elixir.  These source files are compiled into BEAM bytecode, which is then assembled into AtomVM "packbeam" (`.avm`) files.  This packbeam file is flashed onto the ESP32 device, starting at the data partition address `0x210000`.  When AtomVM starts, it will look in this partition for the first occurrence of a BEAM module that exports a `start/0` function.  Once that module is located, execution of the BEAM bytecode will commence at that point.

The following diagram provides a simplified overview of the layout of the AtomVM virtual machine and Erlang/Elixir applications on the ESP32 flash module.

    |               |
    +---------------+  ----------- 0x1000
    | boot loader   |           ^
    +---------------+           |
    | partition map |           | AtomVM
    +---------------+           | binary
    |               |           | image
    |   AtomVM      |           |
    |   Virtual     |           |
    |   Machine     |           |
    |               |           v
    +---------------+  ----------- 0x210000
    |               |           ^
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

> Note. There are a wide variety of ESP32 modules, ranging from home-made breadboard solutions to all-in-one development boards.  For simplicity, we assume a development board that can both be powered by a USB cable and which can be simultaneously flashed using the same cable, e.g., the [Espressif ESP32 DevKit](https://www.espressif.com/en/products/devkits/esp32-devkitc).

Consult your local development board documentation for instructions about how to connect your device to your development machine.

### Deploying the AtomVM virtual machine

The following methods can be used to deploy the AtomVM virtual machine to an ESP32 device:

1. Flashing a binary image;
1. Building from source.

#### Flashing a binary images

Flashing the ESP32 using a pre-built binary image is by far the easiest path to getting started with development on the ESP32.  Binary images contain the virtual machine image and all of the necessary components to run your application.

We recommend first erasing any existing applications on the ESP32 device.  E.g.,

    shell$ esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 erase_flash
    ...

> Note.  Specify the device port and baud settings and AtomVM image name to suit your particular environment.

Next, download the latest stable or nightly ESP32 release image from the [AtomVM web site](http://www.atomvm.net).

> Note.  Nightly images may be unstable and may result in unpredictable behavior.

Finally, use the `esptool` to flash the image to the start address `0x1000` on the ESP32.  E.g.,

    shell$ esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 \
        --before default_reset --after hard_reset \
        write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect \
        0x1000 atomvm-esp32-v0.1.0.bin
    ...

Once completed, your ESP32 device is ready to run Erlang or Elixir programs targeted for AtomVM.

#### Building from source

You may optionally build AtomVM from source and deploy the AtomVM virtual machine to your ESP32 device manually.  Building AtomVM from source is slightly more involved, as it requires the installation of the Espressif IDF SDK and tool chain and is typically recommended only for users who are doing development on the AtomVM virtual machine, or for developers implementing custom Nifs or ports.

Instructions for building AtomVM from source are covered in the AtomVM [Build Instructions](build-instructions.md)

### Deploying an AtomVM application

An AtomVM application is a collection of BEAM files, which have been compiled using the Erlang or Elixir compiler.  These BEAM files are assembled into an AtomVM "packbeam" (`.avm`) file, which in turn is flashed to the `main` data partition on the ESP32 flash module, starting at address `0x210000`.

When the AtomVM virtual machine starts, it will search for the first module that contains an exported `start/0` function in this partition, and it will begin execution of the BEAM bytecode at that function.

AtomVM applications can be written in Erlang or Elixir, or a combination of both.  The AtomVM community has provided tooling for both platforms, making deployment of AtomVM applications as seamless as possible.

This section describes both Erlang and Elixir tooling for deploying AtomVM applications to ESP32 devices.

#### Erlang Tooling

Deployment of AtomVM applications written in the Erlang programming language is supported via the [`atomvm_rebar3_plugin`](https://github.com/fadushin/atomvm_rebar3_plugin) plugin, a community-supported plugin to the [`rebar3`](https://rebar3.org) Erlang build tool.

You can generate a simple application from scratch using the `atomvm_rebar3_plugin` template, as follows:

Edit or create the `$HOME/.config/rebar3/rebar.config` file to include the `atomvm_rebar3_plugin` plugin:

    %% $HOME/.config/rebar3/rebar.config
    {plugins, [
        {atomvm_rebar3_plugin, "0.3.0"},
        ...
    ]}.

In any directory in which you have write permission, issue

    shell$ rebar3 new atomvm_app <app-name>

where `<app-name>` is the name of the application you would like to create (e.g., `myapp`).  This command will generate a rebar project under the directory `<app-name>`.

The generated application will contain the proper `rebar.config` configuration and will contain the `<app-name>.erl` module, which exports the `start/0` function with a stubbed implementation.

Specifically, note the following stanza in the generated `rebar.config` file:

    %% rebar.config
    {plugins, [
        {atomvm_rebar3_plugin, "0.3.0"},
        ...
    ]}.

And note the `myapp` application exports a `start/0` function, e.g.,

    %% erlang
    -module(myapp).
    -export([start/0]).

    start() ->
        ok.

With this plugin installed, you have access to the `esp32_flash` target, which will build an AtomVM packbeam

    shell$ rebar3 esp32_flash --port /dev/ttyUSB0
    ===> Fetching atomvm_rebar3_plugin v0.3.0
    ===> Fetching rebar3_hex v6.11.3
    ===> Fetching hex_core v0.7.1
    ===> Fetching verl v1.0.2
    ===> Analyzing applications...
    ===> Compiling verl
    ===> Compiling hex_core
    ===> Compiling rebar3_hex
    ===> Fetching atomvm_packbeam v0.3.0
    ===> Analyzing applications...
    ===> Compiling atomvm_rebar3_plugin
    ===> Compiling packbeam
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling myapp
    ===> AVM file written to : myapp.avm
    ===> esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect 0x210000 /home/frege/myapp/_build/default/lib/myapp.avm

> Note. Consult the the [`atomvm_rebar3_plugin`](https://github.com/fadushin/atomvm_rebar3_plugin) plugin documentation, for more detailed information about how to use this tool.

Once the application has been flashed, you may connect to the ESP32 over the serial port using `minicom`, `screen`, or equivalent.

#### Elixir Tooling

> TODO mix + https://github.com/atomvm/ExAtomVM + hex

## Getting Started on the STM32 platform

> TODO will se distribute a binary image for STM32?

## Getting Started on the Generic UNIX platform

AtomVM may be run on UNIX-like platforms using the `AtomVM` executable.

You may specify one or more AVM files on the command line when running the `AtomVM` command.  BEAM modules defined in earlier AVM modules on the command line take higher precedence that BEAM modules included in AVM files later in the argument list.

    shell$ AtomVM /path/to/myapp.avm

Currently, the `AtomVM` executable must be built from source.

> See the AtomVM [Build Instructions](build-instructions.md) for instructions about how to build AtomVM on the Generic UNIX platform.

## Where to go from here

The following resources may be useful for understanding how to develop Erlang or Elixir applications for the AtomVM platform:

* [Example Programs](example-programs.md)
* [Programmers Guide](programmers-guide.md)
