<!--
 Copyright 2023 Fred Dushin <fred@dushin.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM Tooling

AtomVM provides an implementation of the Erlang virtual machine, and as such it provides runtime support for applications targeted for the platform.

However, developers will typically make use of downstream tooling that simplifies the development and provisioning of applications onto devices that are running the on the virtual machine.

This chapter presents an overview of these tools and how they can be used to make you more productive as an AtomVM developer.

Two tools are supported, one for Erlang developers, and one for Elixir developers:

* For Erlang developers: [`atomvm_rebar3_plugin`](#atomvm_rebar3_plugin)
* For Elixir developers: [`ExAtomVM`](#exatomvm)


## `atomvm_rebar3_plugin`

The [`atomvm_rebar3_plugin`](https://atomvm.github.io/atomvm_rebar3_plugin) is a [rebar3](https://rebar3.org) plugin that can be used to create and flash Erlang applications that run over AtomVM.  Using this plugin greatly simplies the process of building Erlang applications that run over AtomVM, and is strongly encouraged for all users.

### Prerequisites

To use the [`atomvm_rebar3_plugin`](https://atomvm.github.io/atomvm_rebar3_plugin), you will need the following software on your development machine:

* A suitable version of the [Erlang/OTP](https://www.erlang.org) distribution.  See the [Release Notes](release-notes.md) for information about supported Erlang/OTP versions.
* A recent version of the [rebar3](https://rebar3.org) command-line tool.
* (optional) The [git](https://git-scm.com) command line tool, to follow examples in this chapter.
* For flashing to ESP32, the [`esptool`](https://github.com/espressif/esptool) program.
* For flashing to STM32, `st-flash` via [stlink](https://github.com/stlink-org/stlink)
* (optional) A serial console program such as [minicom](https://en.wikipedia.org/wiki/Minicom) or [screen](https://en.wikipedia.org/wiki/GNU_Screen), to view console output from a device.

### Erlang Example Program

To see this plugin in action, we will clone the [`atomvm_examples`](https://github.com/atomvm/atomvm_examples) Github repository, and build and run the most simple

    shell$ git clone https://github.com/atomvm/atomvm_examples
    ...
    shell$ cd atomvm_examples/erlang/hello_world

From this directory we will run various [rebar3](https://rebar3.org) targets in the steps below.

### Creating an AtomVM AVM file

To create an AtomVM packbeam file (ending in `.avm`), use the `packbeam` target in the `atomvm` namespace:

    shell$ rebar3 atomvm packbeam
    ...
    ===> AVM file written to .../hello_world/_build/default/lib/hello_world.avm

See the [`atomvm_rebar3_plugin`](https://atomvm.github.io/atomvm_rebar3_plugin) page for more detailed instructions about how to use the `packbeam` target.

### Running on the `generic_unix` platform

If you have installed AtomVM on a generic UNIX platform, you and run the example program directly using the `atomvm` command:

    shell$ atomvm _build/default/lib/hello_world.avm
    Hello World
    Return value: ok

For instructions about how to install AtomVM on the `generic_unix` platform, see the [Getting Started Guide](getting-started-guide.md)

### Flashing your application

The [`atomvm_rebar3_plugin`](https://atomvm.github.io/atomvm_rebar3_plugin) supports flash targets for various device types.  These targets are described in more detail below.

#### ESP32

To flash AtomVM AVM file to an ESP32 device, use the `esp32_flash` target in the `atomvm` namespace.  Users will typically specify the device port and baud rate as command-line options to this target.

> Note. In order to use the `esp32_flash` target, you will need to install the [`esptool`](https://github.com/espressif/esptool) program.

For example:

    shell$ rebar3 atomvm esp32_flash --port /dev/ttyUSB0 --baud 921600
    ...
    ===> esptool.py --chip auto --port /dev/ttyUSB0 --baud 921600 --before default_reset --after hard_reset write_flash -u --flash_mode keep --flash_freq keep --flash_size detect 0x210000 /path/to/atomvm_examples/erlang/hello_world/_build/default/lib/hello_world.avm

See the [`atomvm_rebar3_plugin`](https://atomvm.github.io/atomvm_rebar3_plugin) page for more detailed instructions about how to use the `esp32_flash` target.

You can now use a serial console program such as [minicom](https://en.wikipedia.org/wiki/Minicom) or [screen](https://en.wikipedia.org/wiki/GNU_Screen) to view console output from a device.

For example:

        ###########################################################

           ###    ########  #######  ##     ## ##     ## ##     ##
          ## ##      ##    ##     ## ###   ### ##     ## ###   ###
         ##   ##     ##    ##     ## #### #### ##     ## #### ####
        ##     ##    ##    ##     ## ## ### ## ##     ## ## ### ##
        #########    ##    ##     ## ##     ##  ##   ##  ##     ##
        ##     ##    ##    ##     ## ##     ##   ## ##   ##     ##
        ##     ##    ##     #######  ##     ##    ###    ##     ##

        ###########################################################

    I (852) AtomVM: Starting AtomVM revision 0.6.0-alpha.1
    I (862) sys: Loaded BEAM partition boot.avm at address 0x1d0000 (size=262144 bytes)
    I (882) network_driver: Initialized network interface
    I (882) network_driver: Created default event loop
    I (902) AtomVM: Found startup beam esp32init.beam
    I (922) AtomVM: Starting esp32init.beam...
    ---
    AtomVM init.
    I (932) sys: Loaded BEAM partition main.avm at address 0x210000 (size=1048576 bytes)
    Starting application...
    Hello World
    AtomVM finished with return value: ok
    I (972) AtomVM: AtomVM application terminated.  Going to sleep forever ...

#### STM32

To flash AtomVM AVM file to an STM32 device, use the `stm32_flash` target in the `atomvm` namespace.

> Note. In order to use the `stm32_flash` target, you will need to install the  `st-flash` tool from the open source (bsd-3 licensed) [stlink](https://github.com/stlink-org/stlink) suite of stm32 utilities.

Since the AtomVM core libraries are not flashed to an STM32 device, you will need to include is library in your application.  As part of the build process for the STM32, you will have built the AtomVM core libraries into a file named `atomvmlib.avm`

> Note.  It is critical that the version of the AtomVM core libraries match the version of the AtomVM virtual machine you built as part of the STM32 build.  Be sure to use the version of this library (written to `build/lib/atomvmlib.avm` during the build process).  For more information about how to build AtomVM for the STM32 platform, see the AtomVM [Build Instructions](build-instructions.md).

In general, it is also a good idea to use the prune option when creating your application's AVM file.  This way, only the modules that are needed for your application will be included, which will decrease the size of your application's AVM file, leading to faster development times.

Edit the `rebar.config` so that it includes the following `atomvm_rebar3_plugin` stanza, if it does not already.

    {atomvm_rebar3_plugin, [
        {packbeam, [prune]}
    ]}.

This stanza will guarantee that the generated packbeam file will be pruned when created.

You will need to first build a packbeam file that includes the AtomVM core libraries.  Use the `packbeam` task in the `atomvm` namespace, and specify the path to the `atomvmlib.avm` file you created as part of the build.

    shell$ rebar3 atomvm packbeam -e /path/to/atomvmlib.avm

You may now flash your application to your STM32 device:

    shell$ rebar3 atomvm stm32_flash
    ...
    ===> st-flash --reset write /path/to/atomvm_examples/erlang/hello_world/_build/default/lib/hello_world.avm 0x8080000

For devices with only 512KB of flash the application address is different and must be specified:

    shell$ rebar3 atomvm stm32_flash -o 0x8060000
    ...
    ===> st-flash --reset write /path/to/atomvm_examples/erlang/hello_world/_build/default/lib/hello_world.avm 0x8060000

See the [`atomvm_rebar3_plugin`](https://atomvm.github.io/atomvm_rebar3_plugin) page for more detailed instructions about how to use the `stm32_flash` target.

You can now use a serial console program such as [minicom](https://en.wikipedia.org/wiki/Minicom) or [screen](https://en.wikipedia.org/wiki/GNU_Screen) to view console output from a device.

For example:

        ###########################################################

           ###    ########  #######  ##     ## ##     ## ##     ##
          ## ##      ##    ##     ## ###   ### ##     ## ###   ###
         ##   ##     ##    ##     ## #### #### ##     ## #### ####
        ##     ##    ##    ##     ## ## ### ## ##     ## ## ### ##
        #########    ##    ##     ## ##     ##  ##   ##  ##     ##
        ##     ##    ##    ##     ## ##     ##   ## ##   ##     ##
        ##     ##    ##     #######  ##     ##    ###    ##     ##

        ###########################################################

    INFO [51] AtomVM: Starting AtomVM revision 0.6.0-alpha.2+git.59e25c34
    INFO [58] AtomVM: Booting file mapped at: 0x8080000, size: 444
    INFO [64] AtomVM: Starting: hello_world.beam...

    ---
    Hello World
    INFO [74] AtomVM: Exited with return: ok
    INFO [78] AtomVM: AtomVM application terminated.  Going to sleep forever ...

#### Raspberry Pico

To generate a Raspberry Pico uf2 file from an AtomVM AVM file and flash it to an rp2040 device, use the `pico_flash` target in the `atomvm` namespace.

For example:

    shell$ rebar3 atomvm pico_flash
    ...
    ===> AVM file written to /path/to/atomvm_examples/erlang/hello_world/_build/default/lib/hello_world.avm
    ===> Resetting device at path /dev/ttyACM0
    ===> Waiting for the device at path /run/media/${USER}/RPI-RP2 to settle and mount...
    ===> Copying /path/to/atomvm_examples/erlang/hello_world/_build/default/lib/hello.uf2 to /run/media/${USER}/RPI-RP2...

See the [`atomvm_rebar3_plugin`](https://atomvm.github.io/atomvm_rebar3_plugin) page for more detailed instructions about how to use the `pico_flash` target.

You can now use a serial console program such as [minicom](https://en.wikipedia.org/wiki/Minicom) or [screen](https://en.wikipedia.org/wiki/GNU_Screen) to view console output from a device.

For example:

        ###########################################################

           ###    ########  #######  ##     ## ##     ## ##     ##
          ## ##      ##    ##     ## ###   ### ##     ## ###   ###
         ##   ##     ##    ##     ## #### #### ##     ## #### ####
        ##     ##    ##    ##     ## ## ### ## ##     ## ## ### ##
        #########    ##    ##     ## ##     ##  ##   ##  ##     ##
        ##     ##    ##    ##     ## ##     ##   ## ##   ##     ##
        ##     ##    ##     #######  ##     ##    ###    ##     ##

        ###########################################################

    Starting AtomVM revision 0.6.0-alpha.2+git.59e25c34
    Found startup beam hello_world.beam
    Starting hello_world.beam...
    ---
    Hello World
    AtomVM finished with return value: ok
    AtomVM application terminated.  Going to sleep forever ...

## `ExAtomVM`

The [`ExAtomVM`](https://github.com/atomvm/ExAtomVM) tool is a [mix](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html) plugin that can be used to create and flash [Elixir](https://elixir-lang.org) applications that run over AtomVM.  Using this plugin greatly simplies the process of building Elixir applications that run over AtomVM, and is strongly encouraged for new users.

### Prerequisites

To use the [`ExAtomVM`](https://github.com/atomvm/ExAtomVM) tool, you will need the following software on your development machine:

* A suitable version of the [Erlang/OTP](https://www.erlang.org) distribution.  See the [Release Notes](release-notes.md) for information about supported Erlang/OTP versions.
* A suitable version of the [Elixir](https://elixir-lang.org) distribution.  See the [Release Notes](release-notes.md) for information about supported Elixir versions.
* (optional) The [git](https://git-scm.com) command line tool, to follow examples in this chapter.
* For flashing to ESP32, the [`esptool`](https://github.com/espressif/esptool) program.
* (optional) A serial console program such as [minicom](https://en.wikipedia.org/wiki/Minicom) or [screen](https://en.wikipedia.org/wiki/GNU_Screen), to view console output from a device.

### Elixir Example Program

To see this plugin in action, we will clone the [`atomvm_examples`](https://github.com/atomvm/atomvm_examples) Github repository, and build and run the most simple

    shell$ git clone https://github.com/atomvm/atomvm_examples
    ...
    shell$ cd atomvm_examples/elixir/HelloWorld

From this directory we will run various [mix](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html) targets in the steps below.

### Creating an AtomVM AVM file

To create an AtomVM packbeam file (ending in `.avm`), first use the `mix.deps` target to [mix](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html) in order to download any dependencies:

    shell$ mix deps.get
    * Updating exatomvm (https://github.com/atomvm/ExAtomVM/)
    remote: Enumerating objects: 17, done.
    remote: Counting objects: 100% (17/17), done.
    remote: Compressing objects: 100% (10/10), done.
    remote: Total 17 (delta 6), reused 16 (delta 6), pack-reused 0
    origin/HEAD set to master

You can now use the `atomvm.packbeam` target to create a packbeam (ending in `.avm`) file:

    shell$ mix atomvm.packbeam
    ==> exatomvm
    Compiling 5 files (.ex)
    Generated exatomvm app
    ==> HelloWorld
    Compiling 1 file (.ex)
    Generated HelloWorld app
    No avm_deps directory found.
    This message can be safely ignored when standard libraries are already flashed to lib partition.

The `HelloWorld.avm` is located in the top level directory:

    shell$ ls -l HelloWorld.avm
    -rw-rw-r--  1 user  wheel  19120 Oct 13 14:06 HelloWorld.avm

See the [`ExAtomVM`](https://github.com/atomvm/ExAtomVM) page for more detailed instructions about how to use the `atomvm.packbeam` target.

### Running on the `generic_unix` platform

If you have installed AtomVM on a generic UNIX platform, you and run the example program directly using the `atomvm` command:

    shell$ atomvm HelloWorld.avm
    Hello World
    Return value: ok

For instructions about how to install AtomVM on the `generic_unix` platform, see the [Getting Started Guide](getting-started-guide.md)

### Flashing your application

The [`ExAtomVM`](https://github.com/atomvm/ExAtomVM) plugin supports flash targets for various device types.  These targets are described in more detail below.

> Note.  Currently, the [`ExAtomVM`](https://github.com/atomvm/ExAtomVM) tool only supports flash targets for the ESP32 platform.

#### ESP32

To flash AtomVM packbeam file to an ESP32 device, use the `mix.esp32.flash` target.  Users will typically specify the device port and baud rate as command-line options to this target.

> Note. In order to use the `mix.esp32.flash` target, you will need to install the [`esptool`](https://github.com/espressif/esptool) program.

For example:

    shell$ mix atomvm.esp32.flash --port /dev/ttyUSB0 --baud 921600
    ...

See the [`ExAtomVM`](https://github.com/atomvm/ExAtomVM) page for more detailed instructions about how to use the `mix.esp32.flash` target.

You can now use a serial console program such as [minicom](https://en.wikipedia.org/wiki/Minicom) or [screen](https://en.wikipedia.org/wiki/GNU_Screen) to view console output from a device.

For example:

        ###########################################################

           ###    ########  #######  ##     ## ##     ## ##     ##
          ## ##      ##    ##     ## ###   ### ##     ## ###   ###
         ##   ##     ##    ##     ## #### #### ##     ## #### ####
        ##     ##    ##    ##     ## ## ### ## ##     ## ## ### ##
        #########    ##    ##     ## ##     ##  ##   ##  ##     ##
        ##     ##    ##    ##     ## ##     ##   ## ##   ##     ##
        ##     ##    ##     #######  ##     ##    ###    ##     ##

        ###########################################################

    I (852) AtomVM: Starting AtomVM revision 0.6.0-alpha.1
    I (862) sys: Loaded BEAM partition boot.avm at address 0x1d0000 (size=262144 bytes)
    I (882) network_driver: Initialized network interface
    I (882) network_driver: Created default event loop
    I (902) AtomVM: Found startup beam esp32init.beam
    W (902) sys: AVM partition not found for lib.avm
    I (902) AtomVM: Unable to mount lib.avm partition. Hopefully the AtomVM core libraries are included in your application.
    I (922) AtomVM: Starting esp32init.beam...
    ---
    AtomVM init.
    I (932) sys: Loaded BEAM partition main.avm at address 0x210000 (size=1048576 bytes)
    Starting application...
    Hello World
    AtomVM finished with return value: ok
    I (972) AtomVM: AtomVM application terminated.  Going to sleep forever ...

## `atomvm_packbeam`

The [atomvm_packbeam](https://atomvm.github.io/atomvm_packbeam) tool is a simple command-line utility that allows you to create, inspect, and manipulate AtomVM [PackBEAM](packbeam-format.md) files.  By convention, PackBEAM files end in the `.avm` suffix and are referred to as "AVM" files, in the remainder of this section.

> Note.  Users generally do not have a need to use the `packbeam` tool directly.  Instead, the functionality of this tool is embedded in the [`atomvm_rebar3_plugin`](https://atomvm.github.io/atomvm_rebar3_plugin).

### Installation

Consult the [atomvm_packbeam](https://atomvm.github.io/atomvm_packbeam) Github page for instructions about how to install the `atomvm_packbeam` utility.  Once installed, you should have the `packbeam` command line tool available in your `PATH`.

### Usage

The `packbeam` command supports the following sub-commands:

* `create`  Create an AVM file from a collection of files.
* `list`  List the contents of an AVM file.
* `extract`  Extract elements from an AVM file.
* `delete`  Delete elements from an AVM file.

These sub-commands are described in more detail below.

> These notes provide only a high-level view of this `packbeam` utility.  For more detailed information, see the [atomvm_packbeam](https://atomvm.github.io/atomvm_packbeam) Github repository.

#### Creating AVM files

To create an AVM from a list of existing files (typically `.beam` files), use the `create` subcommand.  Specify the output `.avm` first, followed by a list of files you would like to include in the output file.  E.g.,

    shell$ packbeam create output.avm foo.beam bar.beam

> Note.  Creation of AVM files is more typically done via the [atomvm_rebar3_plugin](https://atomvm.github.io/atomvm_rebar3_plugin); however, the `packbeam` command can be used to inspect and/or manipulate AVM files after they have been created by this plugin.  This isn't typically required, but in some instances it can be useful.

Note that you can supply a previously created AVM file as an input to another creation, which will result in including all the files in the source AVM file in the destination.

    shell$ packbeam create new_output.avm tapas.beam output.avm

You can also embed non-BEAM files in an AVM file.  These files are accessible programmatically withing atomvm via the `atomvm:read_priv/2` function, described in the AtomVM [Programmer's Guide](programmers-guide.md).

For example, if you wanted to add a file `my_app/priv/my_file.txt` to a new file, you could use the following command:

    shell$ packbeam create my_app.beam my_app/priv/my_file.txt my_lib.avm

> Note.  There are conventions for embedding non-BEAM files in AVM files that need to be followed in order to be able to load these files programmatically within AtomVM.  Generally, these files must obey the path `<module-name>/priv/<path-to-file>`, where `<module-name>` is the name of a module, and `<path-to-file>` is a path to the embedded file.  (This path may include embedded `/` separators).  Example: `my_app/priv/bubbles/sample.txt`

##### Start Flags

An AtomVM application must contain a start entrypoint, i.e., a module that exports the `start/0` function.  You can specify the name of this module via the `--start` flag.  E.g.,

    shell$ packbeam create --start main my_app.avm foo.beam bar.beam main.beam

Use of this flag will ensure that the `main.beam` module will be found first in the search order when the AtomVM virtual machine starts your application.

##### Pruning

Pruning an AVM file is a useful mechanism for making your AVM files smaller, and thus faster to flash and including less data than necessary.  You can prune an AVM using the

    shell% packbeam create --start main --prune my_app.beam foo.beam bar.beam main.beam a.beam b.beam c.beam

Any BEAM files that contain no transitive references from the start module are removed from the output AVM file, making them smaller and less bloated.

> Note.  You can only use the `--prune` option if you specify a `--start` module.

#### Listing AVM file contents

You can list the contents of an AVM file via the `list` sub-command.

    shell$ packbeam list myapp.avm
    myapp.beam * [384]
    myapp/priv/application.bin [220]

Any BEAM files with an exported `start/0` function are listed with an asterisk (`*`).  In general, if you want your application to start from a designated entrypoint, that BEAM file should occur first in the list.

The size (in bytes) of the entries are listed in square brackets (`[]`).

#### Extracting AVM file contents

You can extract elements of an AVM file, writing them to the file system, using the `extract` sub-command.

Specify the directory location into which you would like to extract the files using the `-out` flag, followed by the path to the input AVM file, and a list of paths from the input AVM you would like to extract.

    shell$ mkdir mydir
    shell$ packbeam extract -out mydir myapp.avm myapp/priv/application.bin
    Writing to mydir ...
    x myapp/priv/application.bin

#### Deleting AVM file contents

You can delete elements of an AVM file using the `delete` sub-command.

Specify the AVM file you would like to write as output (which can be the same as the input AVM file) using the `-out` flag, followed by the path to the input AVM file, and a list of paths from the input AVM you would like to delete.

    shell$ packbeam delete -out myapp2.avm myapp.avm myapp/priv/application.bin

    shell$ packbeam list myapp2.avm
    myapp.beam * [384]


### Help

To get help about `packbeam` syntax, use the `help` subcommand:

    shell$ packbeam help

    packbeam version 0.7.0

    Syntax:
        packbeam <sub-command> <options> <args>

    The following sub-commands are supported:

        create <options> <output-avm-file> [<input-file>]+
            where:
            <output-avm-file> is the output AVM file,
            [<input-file>]+ is a list of one or more input files,
            and <options> are among the following:
                [--prune|-p]           Prune dependencies
                [--start|-s <module>]  Start module
                [--remove_lines|-r]    Remove line number information from AVM files

        list <options> <avm-file>
            where:
            <avm-file> is an AVM file,
            and <options> are among the following:
                [--format|-f csv|bare|default]  Format output

        extract <options> <avm-file> [<element>]*
            where:
            <avm-file> is an AVM file,
            [<element>]+ is a list of one or more elements to extract
                (if empty, then extract all elements)
            and <options> are among the following:
                [--out|-o <output-directory>]   Output directory into which to write elements
                (if unspecified, use the current working directory)

        delete <options> <avm-file> [<element>]+
            where:
            <avm-file> is an AVM file,
            [<element>]+ is a list of one or more elements to delete,
            and <options> are among the following:
                [--out|-o <output-avm-file>]    Output AVM file

        version
            Print version and exit

        help
            Print this help

> For more detailed information about the [atomvm_packbeam](https://atomvm.github.io/atomvm_packbeam) utility, see the [atomvm_packbeam](https://atomvm.github.io/atomvm_packbeam) Github page.

## Where to go from here

With knowledge of AtomVM tooling, you can more easily follow the AtomVM [Example Programs](https://github.com/atomvm/atomvm_examples)
