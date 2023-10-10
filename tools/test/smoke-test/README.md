<!---
  Copyright 2023 Fred Dushin <fred@dushin.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

Welcome to smoke testing scripts for AtomVM!

The scripts in this project tree are designed to simply smoke testing of example programs on various platforms (e.g., ESP32), so that testers can focus on the results of smoke tests, rather than the details of how to set up a testing environment.  Hopefully, this will make your life simpler.

In order to use these scripts, you must first execute the `bin/relocate` script in the bin directory of this project tree.  This command will generate an `activate` script, which you should then source (_not_ execute).  Currently, only the `bash` shell is supported.

> Note that the `relocate` script only needs to be executed once, unless you change the location of the project tree.  After you run `relocate`, you can source the `activate` script from as many shells as you like.  The `activate` script will detect if it has already been run and will not change any settings if it has already run from a given shell.

Sourcing the `activate` script will prompt you for default environment variable values, which you might find useful for your testing environment (e.g., the device port and baud rate), so these values do not need to be constantly re-entered.  To accept the default, simply hit the enter key and the specified default will not be changed.

Once the activate script is sourced, your PATH environment variable will be prefixed with the path to the `bin` directory of this project tree.  Your prompt will also be prefixed with the string `[atomvm-smoke]` to help identify the fact that you are in this environment.

> Note that currently there is no support for deactivation.  You should just terminate the shell you have used to activate this project, in order to clear any settings.

# Commands

This project includes the following commands.

## `esp32-erase`

Use the `esp32-erase` command to erase the flash on an ESP32.  You should always erase the flash before beginning any smoke testing on an ESP32, in order to ensure that the flash chip is starting from a fresh state.

The syntax for this command is as follows:

    Syntax: esp32-erase [-h] [-p <port>] [-b <baud>]

Any environment variables set up during activation will be used by this command.  You may use the above command line options to override any environment variable settings.

## `esp32-flash`

Use the `esp32-flash` command to flash an ESP32 device with an AtomVM image, which presumably you will have downloaded from the AtomVM release repository.

The syntax for this command is as follows:

    Syntax: esp32-flash [-h] [-p <port>] [-b <baud>] <path-to-atomvm-image>

Any environment variables set up during activation will be used by this command.  You may use the above command line options to override any environment variable settings.

# Example Usage

The following steps illustrate a common smoke test scenario.

First, assume this project is installed in `/path/to/project`.

Start by executing the `relocate` script:

    shell$ /path/to/project/bin/relocate
    %%
    %% Created '/path/to/project/bin/activate' script.
    %%
    %% Source this file to start smoke testing.
    %%
    %% Example:
    %%
    %%     shell$ . '/path/to/project/bin/activate'
    %%

> Note.  Remember, this step only needs to be performed _once_, unless you change the location of the project.

This step will create an activate script, which you should then _source_ (*not* execute):

    shell$ . /path/to/project/bin/activate

    Enter any changes to default settings (hit enter to accept defaults)
    ====================================================================
    ESP32_PORT [/dev/ttyUSB0]> /dev/tty.usbserial-0164A0E0
    ESP32_BAUD [921600]>
    ESP32_ESPTOOL [esptool.py]>
    ESP32_CHIP [auto]>
    ESP32_OFFSET [0x1000]>
    ==================================

    "/path/to/project/bin" has been added to your PATH environment variable.

    The following variables have been added to your environment:

    ESP32_CHIP=auto
    ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_PORT=/dev/tty.usbserial-0164A0E0
    ESP32_ESPTOOL=esptool.py
    ESP32_OFFSET=0x1000
    ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_CHIP=auto
    ESP32_BAUD=921600
    ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_BAUD=921600
    ESP32_PORT=/dev/tty.usbserial-0164A0E0
    ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_ESPTOOL=esptool.py

    You are now ready for smoke testing AtomVM.  Good luck!

You can now run commands, such as erasing the flash drive on your device:

    shell$ esp32-erase
    # esptool.py --chip auto --port /dev/tty.usbserial-0164A0E0 --baud 921600 erase_flash
    esptool.py v3.3.4-dev
    Serial port /dev/tty.usbserial-0164A0E0
    Connecting....
    Detecting chip type... Unsupported detection protocol, switching and trying again...
    Connecting.....
    Detecting chip type... ESP32
    Chip is ESP32-D0WDQ6 (revision v1.0)
    Features: WiFi, BT, Dual Core, Coding Scheme None
    Crystal is 40MHz
    MAC: 30:ae:a4:44:17:04
    Uploading stub...
    Running stub...
    Stub running...
    Changing baud rate to 921600
    Changed.
    Erasing flash (this may take a while)...
    Chip erase completed successfully in 3.9s
    Hard resetting via RTS pin...

And then flash the image to your device:

    shell$ esp32-flash /path/to/atomvm.img
    # esptool.py --chip auto --port /dev/tty.usbserial-0164A0E0 --baud 921600 --before default_reset --after hard_reset write_flash -u --flash_mode keep --flash_freq keep --flash_size detect 0x1000 /path/to/atomvm.img
    esptool.py v3.3.4-dev
    Serial port /dev/tty.usbserial-0164A0E0
    Connecting....
    Detecting chip type... Unsupported detection protocol, switching and trying again...
    Connecting....
    Detecting chip type... ESP32
    Chip is ESP32-D0WDQ6 (revision v1.0)
    Features: WiFi, BT, Dual Core, Coding Scheme None
    Crystal is 40MHz
    MAC: 30:ae:a4:44:17:04
    Uploading stub...
    Running stub...
    Stub running...
    Changing baud rate to 921600
    Changed.
    Configuring flash size...
    Auto-detected Flash size: 4MB
    Flash will be erased from 0x00001000 to 0x001eefff...
    Wrote 2031616 bytes at 0x00001000 in 27.4 seconds (593.3 kbit/s)...
    Hash of data verified.

    Leaving...
    Hard resetting via RTS pin...

Your ESP32 device is now flashed with the ESP32 image you specified.

> Note that in the current implementation of AtomVM, you can connect to the ESP32 over a serial monitor, and you will see that the device will start in "development mode", with an AP access point named `AtomVM-ESP32`.  Consult the source code for more information about this AP.

# Smoke testing AtomVM examples

The [`atomvm_examples`](https://github.com/atomvm/atomvm_examples) repo is a good place to start smoke testing.  We assume you know how to clone this repo using git.

Note that you will need a supported OTP version and at least a recent version of the [rebar3](https://rebar3.org) tool.  If you want to smoke test Elixir programs, you should also have a comaptible version of the Elixir compiler and runtime available.  Consult the AtomVM [Relaase Notes](https://www.atomvm.net/doc/master/release-notes.html) for compatibility information.

To run an example program, `cd` into the appropriate example directory, and build and flash the program.  We will show an example with the [`hello_world`](https://github.com/atomvm/atomvm_examples/tree/master/erlang/hello_world) program.

    shell$ cd erlang/hello_world
    shell$ rm -rf _build && rebar3 packbeam -p && rebar3 esp32_flash
    ===> Fetching atomvm_rebar3_plugin v0.6.1
    =WARNING REPORT==== 9-Oct-2023::15:38:40.066552 ===
    Description: "Authenticity is not established by certificate path validation"
        Reason: "Option {verify, verify_peer} and cacertfile/cacerts is missing"

    ===> Fetching rebar3_hex v7.0.7
    ===> Fetching hex_core v0.8.4
    ===> Fetching verl v1.1.1
    ===> Analyzing applications...
    ===> Compiling verl
    ===> Compiling hex_core
    ===> Compiling rebar3_hex
    ===> Fetching atomvm_packbeam v0.6.1
    ===> Fetching rebar3_proper v0.12.1
    ===> Analyzing applications...
    ===> Compiling rebar3_proper
    ===> Analyzing applications...
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    ===> Fetching erlfmt v1.2.0
    ===> Analyzing applications...
    ===> Compiling erlfmt
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling hello_world
    ===> AVM file written to : hello_world.avm
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling hello_world
    ===> esptool.py --chip auto --port /dev/tty.usbserial-0164A0E0 --baud 921600 --before default_reset --after hard_reset write_flash -u --flash_mode keep --flash_freq keep --flash_size detect 0x210000 .../atomvm_examples/erlang/hello_world/_build/default/lib/hello_world.avm

    esptool.py v3.3.4-dev
    Serial port /dev/tty.usbserial-0164A0E0
    Connecting.....
    Detecting chip type... Unsupported detection protocol, switching and trying again...
    Connecting....
    Detecting chip type... ESP32
    Chip is ESP32-D0WDQ6 (revision v1.0)
    Features: WiFi, BT, Dual Core, Coding Scheme None
    Crystal is 40MHz
    MAC: 30:ae:a4:44:17:04
    Uploading stub...
    Running stub...
    Stub running...
    Changing baud rate to 921600
    Changed.
    Configuring flash size...
    Auto-detected Flash size: 4MB
    Flash will be erased from 0x00210000 to 0x00210fff...
    Writing at 0x00210000... (100 %)
    Wrote 16384 bytes at 0x00210000 in 0.2 seconds (557.7 kbit/s)...
    Hash of data verified.

    Leaving...
    Hard resetting via RTS pin...

> Note.  The environment variables set during activation should not require you to set any specific port or baud rates.  However, these settings can be changed on the command line, if desired.

You should now be able to connect to your ESP32 device using a tool like `minicom` or `screen` to check the output on the console.

For example:

    shell$ minicom -D ${ESP32_PORT}

    I (0) cpu_start: Starting scheduler on APP CPU.

    ###########################################################

       ###    ########  #######  ##     ## ##     ## ##     ##
      ## ##      ##    ##     ## ###   ### ##     ## ###   ###
     ##   ##     ##    ##     ## #### #### ##     ## #### ####
    ##     ##    ##    ##     ## ## ### ## ##     ## ## ### ##
    #########    ##    ##     ## ##     ##  ##   ##  ##     ##
    ##     ##    ##    ##     ## ##     ##   ## ##   ##     ##
    ##     ##    ##     #######  ##     ##    ###    ##     ##

    ###########################################################

    I (780) AtomVM: Starting AtomVM revision 0.6.0-alpha.1
    I (790) sys: Loaded BEAM partition boot.avm at address 0x1d0000 (size=262144 bytes)
    I (810) network_driver: Initialized network interface
    I (810) network_driver: Created default event loop
    I (830) AtomVM: Found startup beam esp32init.beam
    I (830) AtomVM: Starting esp32init.beam...
    ---
    AtomVM init.
    I (840) sys: Loaded BEAM partition main.avm at address 0x210000 (size=1048576 bytes)
    Starting application...
    Hello World
    AtomVM finished with return value: ok
    I (880) AtomVM: AtomVM application terminated.  Going to sleep forever ...

Congratulations!  You have completed your first smoke test!
