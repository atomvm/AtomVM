<!--
 Copyright 2021-2022 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Getting Started Guide

The getting started is broken up into the following sections:

* [Getting Started on the ESP32 platform](#getting-started-on-the-esp32-platform)
* [Getting Started on the STM32 platform](#getting-started-on-the-stm32-platform)
* [Getting Started on the Raspberry Pi Pico platform](#getting-started-on-the-raspberry-pi-pico-platform)
* [Getting Started on the Generic UNIX platform](#getting-started-on-the-generic-unix-platform)

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
* An [Erlang/OTP](https://erlang.org);
* A serial console program, such as `minicom` or `screen`, so that you can view console output from your AtomVM application.
* (recommended) For Erlang programs, [`rebar3`](https://rebar3.org);
* (recommended) For Elixir programs, [`mix`](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html), which ships with the Elixir runtime;

For information about specific versions of required software, see the [Release Notes](./release-notes.md).

### Deployment Overview

The ES32 AtomVM virtual machine is an IDF application that runs on the ESP32 platform.  As an IDF application, it provides the object code to boot the ESP device and execute the AtomVM virtual machine code, which in turn is responsible for execution of an Erlang/Elixir application.

The AtomVM virtual machine is implemented in C, and the AtomVM binary image contains the binary object code compiled from C source files, as well as the ESP boot loader and partition map, which tells the ESP32 how the flash module is laid out.

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

Next, download a stable or latest development ESP32 [release image](https://github.com/atomvm/AtomVM/releases).

> Note.  Development images may be unstable and may result in unpredictable behavior.

Finally, use the `esptool` to flash the image to the start address `0x1000` on the ESP32.  E.g.,

    shell$ esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 \
        --before default_reset --after hard_reset \
        write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect \
        0x1000 atomvm-esp32-v0.1.0.bin
    ...

Once completed, your ESP32 device is ready to run Erlang or Elixir programs targeted for AtomVM.

#### Building from source

You may optionally build AtomVM from source and deploy the AtomVM virtual machine to your ESP32 device manually.  Building AtomVM from source is slightly more involved, as it requires the installation of the Espressif IDF SDK and tool chain and is typically recommended only for users who are doing development on the AtomVM virtual machine, or for developers implementing custom Nifs or ports.

Instructions for building AtomVM from source are covered in the AtomVM [Build Instructions](./build-instructions.md)

### Deploying an AtomVM application

An AtomVM application is a collection of BEAM files, which have been compiled using the Erlang or Elixir compiler.  These BEAM files are assembled into an AtomVM "packbeam" (`.avm`) file, which in turn is flashed to the `main` data partition on the ESP32 flash module, starting at address `0x210000`.

When the AtomVM virtual machine starts, it will search for the first module that contains an exported `start/0` function in this partition, and it will begin execution of the BEAM bytecode at that function.

AtomVM applications can be written in Erlang or Elixir, or a combination of both.  The AtomVM community has provided tooling for both platforms, making deployment of AtomVM applications as seamless as possible.

This section describes both Erlang and Elixir tooling for deploying AtomVM applications to ESP32 devices.

#### Erlang Tooling

Deployment of AtomVM applications written in the Erlang programming language is supported via the [`atomvm_rebar3_plugin`](https://github.com/atomvm/atomvm_rebar3_plugin) plugin, a community-supported plugin to the [`rebar3`](https://rebar3.org) Erlang build tool.

You can generate a simple application from scratch using the `atomvm_rebar3_plugin` template, as follows:

Edit or create the `$HOME/.config/rebar3/rebar.config` file to include the `atomvm_rebar3_plugin` plugin:

    %% $HOME/.config/rebar3/rebar.config
    {plugins, [
        atomvm_rebar3_plugin,
        ...
    ]}.

In any directory in which you have write permission, issue

    shell$ rebar3 new atomvm_app <app-name>

where `<app-name>` is the name of the application you would like to create (e.g., `myapp`).  This command will generate a rebar project under the directory `<app-name>`.

The generated application will contain the proper `rebar.config` configuration and will contain the `<app-name>.erl` module, which exports the `start/0` function with a stubbed implementation.

Specifically, note the following stanza in the generated `rebar.config` file:

    %% rebar.config
    {plugins, [
        atomvm_rebar3_plugin,
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
    ===> Fetching atomvm_rebar3_plugin v0.6.0
    ===> Fetching rebar3_hex v6.11.3
    ===> Fetching hex_core v0.7.1
    ===> Fetching verl v1.0.2
    ===> Analyzing applications...
    ===> Compiling verl
    ===> Compiling hex_core
    ===> Compiling rebar3_hex
    ===> Fetching atomvm_packbeam v0.6.0
    ===> Analyzing applications...
    ===> Compiling atomvm_rebar3_plugin
    ===> Compiling packbeam
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling myapp
    ===> AVM file written to : myapp.avm
    ===> esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect 0x210000 /home/frege/myapp/_build/default/lib/myapp.avm

> Note. Consult the the [`atomvm_rebar3_plugin`](https://github.com/atomvm/atomvm_rebar3_plugin) plugin documentation, for more detailed information about how to use this tool.

Once the application has been flashed, you may connect to the ESP32 over the serial port using `minicom`, `screen`, or equivalent.

#### Elixir Tooling

> TODO mix + https://github.com/atomvm/ExAtomVM + hex

## Getting Started on the STM32 platform

AtomVM can run on a wide variety of STM32 chipsets available from [STMicroelectronics](https://www.st.com). The support is not nearly as mature as for the ESP32 platform, but work is ongoing, and pull requests are always welcome. At this time AtomVM will work on any board with a minimum of around 128k ram and 512k (1M recommended) flash. Simple applications and tests have been successfully run on a stm32f411ceu6 (A.K.A. Black Pill V2). These minimum requirements may need to be raised as platform support matures.

### Prerequisites

* [st-flash](https://github.com/texane/stlink), to flash both AtomVM and your packed AVM applications. Make sure to follow its [installation procedure](https://github.com/texane/stlink#installation) before proceeding further.
* [`packbeam`](https://github.com/atomvm/atomvm_packbeam) the AtomVM for packing and stripping `*.beam` files into the AtomVM `*.avm` format.
* A serial console program, such as `minicom` or `screen`, so that you can view console output from your AtomVM application.

### Build an AtomVM binary
You will first need to build a binary configured for your processor and board layout. Consult the [Build Instruction for STM32](./build-instructions#building-for-stm32)

### Flashing
To flash AtomVM, use

    $ st-flash --reset write AtomVM-stm32f407vgt6.bin 0x8000000

To flash your packed AVM, use

    $ st-flash --reset write /path/to/your/packed.avm 0x8080000

You must include the atomvmlib.avm with your application when using [packbeam](https://github.com/atomvm/atomvm_packbeam), and it should be pruned:

	$ packbeam create -p -i application.avm application.beam /path/to/AtomVM/build/libs/atomvmlib.avm

> Note: The option`-i` will instruct packbeam to include file names and line numbers in stack traces. This makes debugging applications far easier, but also increases size, so it may be omitted if desired. The `-p` option should be used, it instructs packbeam to prune the unused functions from the packed `.avm` file, and is strongly recommended.

AtomVM expects to find the AVM at the address 0x808000. On a STM32 Discovery board this means that the 1MB of flash will be split in 512KB available for the program and 512KB available for the packed AVM. If for any reason you want to modify this, you can change `AVM_ADDRESS` and `AVM_FLASH_MAX_SIZE` defines in `main.c`.

### Printing
By default, stdout and stderr are printed on USART2. On the STM32F4Discovery board, you can see them
using a TTL-USB with the TX pin connected to board's pin PA2 (USART2 RX). Baudrate is 115200 and serial transmission
is 8N1 with no flow control.

### Distributed Binaries
Due to the very large number of supported chipsets, the wide variety of board configurations, and the code changes required to support them, it is unlikely pre-built binaries will be available for the stm32 platform in the near future. Consult the [Build Instruction](./build-instructions#building-for-stm32) to create a binary compatible with your board.

## Getting Started on the Raspberry Pi Pico platform

### Prerequisites

None of these tools are strictly required, but all are recommended for easier development:
* [`rebar3`](https://rebar3.org)
* [`atomvm_rebar3_plugin`](https://github.com/atomvm/atomvm_rebar3_plugin)
* [`packbeam`](https://github.com/atomvm/atomvm_packbeam) the AtomVM for packing and stripping `*.beam` files into the AtomVM `*.avm` format. (included as part of the `atomvm_rebar3_plugin`)
* A serial console program, such as `minicom` or `screen`, so that you can view console output from your AtomVM application.

### Building AtomVM for Raspberry Pico

If you want to use a custom built VM for testing consult the [Build Instructions for Raspberry Pi Pico](./build-instructions#building-for-raspberry-pi-pico)

### Installing AtomVM and programs on Raspberry Pico

The approach consists in installing various uf2 files which include the
address they should be loaded to.

You typically need three uf2 files:
- `AtomVM.uf2` for the VM
- `atomvmlib.uf2` for the standard libraries
- your application's uf2.

We provide an escript-based (what else?) tool to build uf2 files called
`uf2tool` that you can use to bundle your `avm` into uf2.

If you need to upgrade AtomVM or the standard libraries, simply copy them again.

### Installing AtomVM on Raspberry Pico

VM binary is file `AtomVM.uf2` - `src/platforms/rp2040/build/src/AtomVM.uf2` if build from source. Simply copy it
to the pico. The VM will crash because there is no application.

### Installing atomvm library to Raspberry Pico

AtomVM library must be installed as well. For Build instructions consult the Raspberry Pi Pico [libAtomVM build steps](./build-instructions#libatomvm-build-steps)

#### Installing it

The library to install is `atomvmlib.uf2`, `build/libs/atomvmlib.uf2` if build from source. Copy the library to the pico.

### Running Hello Pico

This example will print a Hello Pico message repeatedly.

It is built into `build/examples/erlang/rp2040/hello_pico.uf2`.

You can install it and then connect to the serial port with minicom.

### Running your own BEAM code on Raspberry Pico

You need to create an avm file using the `packbeam` tool the [`atomvm_rebar3_plugin`](https://github.com/atomvm/atomvm_rebar3_plugin).

    packbeam create -p -i packed.avm module.beam

or

    rebar3 packbeam -p -i packed.avm module.beam

Then the BEAM file must be converted to UF2.
The VM currently expects the application to be loaded at address 0x10100000.

    ./uf2tool create -o packed.uf2 -s 0x10100000 packed.avm

Copy this UF2 to the Pico after you copied the VM (`AtomVM.uf2`) and the
standard libraries (`atomvmlib.uf2`).


## Getting Started on the Generic UNIX platform

AtomVM may be run on UNIX-like platforms using the `atomvm` command.

You may specify one or more AVM files on the command line when running the `atomvm` command.  BEAM modules defined in earlier AVM modules on the command line take higher precedence that BEAM modules included in AVM files later in the argument list.

    shell$ atomvm /path/to/myapp.avm

To get the current version of AtomVM, use the `-v` option, e.g.:

    shell$ atomvm -v
    0.6.1

Use the `-h` option to get command line help:

    shell$ atomvm -h

    Syntax:

        /usr/local/lib/atomvm/AtomVM [-h] [-v] <path-to-avm-file>+

    Options:

        -h         Print this help and exit.
        -v         Print the AtomVM version and exit.

    Supply one or more AtomVM packbeam (.avm) files to start your application.

    Example:

        $ /usr/local/lib/atomvm/AtomVM /path/to/my/application.avm /path/to/atomvmlib.avm

Currently, the `atomvm` command and libraries must be built and installed from source.

> See the AtomVM [Build Instructions](./build-instructions.md) for instructions about how to build AtomVM on the Generic UNIX platform.

## Where to go from here

The following resources may be useful for understanding how to develop Erlang or Elixir applications for the AtomVM platform:

* [Example Programs](./example-programs.md)
* [Programmers Guide](./programmers-guide.md)


## Getting Started with AtomVM WebAssembly port for NodeJS

AtomVM's WebAssembly port for NodeJS may be run using `node` command and AtomVM.js, AtomVM.worker.js and AtomVM.wasm files.

    shell$ node /path/to/AtomVM.js /path/to/myapp.avm

## Getting Started with AtomVM WebAssembly port for browsers

AtomVM may also be run in modern browsers (Safari, Chrome and Chrome-based, Firefox) using AtomVM.js, AtomVM.worker.js and AtomVM.wasm files.

Please note that these files are different from the NodeJS ones.

Because AtomVM uses SharedArrayBuffer, to be executed by a browser, these files need to be served:
- on localhost or over HTTPS
- by a web server that also sends `Cross-Origin-Opener-Policy` and `Cross-Origin-Embedder-Policy` headers. These headers are also called COOP and COEP headers.

These security requirements are documented in [Mozilla's documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer#security_requirements).

### Trying locally from AtomVM source tree

If you compile AtomVM for Unix as well as for Node as explained in the [build instructions](./build-instructions.md), you can use an AtomVM-based toy webserver to serve the WebAssembly examples with:

```
./src/AtomVM examples/emscripten/wasm_webserver.avm
```

This web server serves HTML files from `examples/emscripten/`. It works without HTTPS because files are served on localhost.

### Using a hosting service with a `_headers` file

You can also host the three files on a hosting service such as Netlify that uses `_headers` files.

The file could have the following content:

```
/*
  Cross-Origin-Opener-Policy: same-origin
  Cross-Origin-Embedder-Policy: require-corp
```

### Using web server such as Nginx

You can also host the three files on web server such as Nginx or Apache.

The configuration for Nginx would be:

```
server {
   add_header Cross-Origin-Opener-Policy "same-origin";
   add_header Cross-Origin-Embedder-Policy "require-corp";
   location / {
       ...
   }
}
```

### Using Javascript service worker trick

If you have no possibility to modify the headers, for example with GitHub pages, you can still get AtomVM to run in the browser using a Javascript service worker trick.

We did successfully use [coi-serviceworker](https://github.com/gzuidhof/coi-serviceworker).

## Where to go from here

The following resources may be useful for understanding how to develop Erlang or Elixir applications for the AtomVM platform:

* [Example Programs](./example-programs.md)
* [Programmers Guide](./programmers-guide.md)
