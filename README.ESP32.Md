<!--
 Copyright 2018-2020 Davide Bettio <davide@uninstall.it>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

Requirements
============

AtomVM has been tested with esp-idf v3.2.

Building AtomVM for ESP32
=========================

* build:

```
cd src/platforms/esp32/
idf.py set-target esp32 (or esp32c3, esp32s2, esp32s3)
idf.py menuconfig
idf.py build
```

* flash:
```
./build/mkimage.sh
./build/flashimage.sh
```

Running BEAM code on ESP32
==========================

* BEAM module must be stripped before using PackBEAM:

```
./PackBEAM packed.avm module.beam
```

* Packed BEAM module should be written to dedicated BEAM partition:

```
$IDF_PATH/components/esptool_py/esptool/esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect  0x210000 packed.avm
```

Running Hello World
===================

This example will print a Hello World message.

```
# Flash the hello_world.avm to the device
$ $IDF_PATH/components/esptool_py/esptool/esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect  0x210000 build/examples/erlang/hello_world.avm

$ cd src/platforms/esp32

# Open the serial monitor
$ idf.py monitor
```

Running Blink Example
=====================

This example will switch on and off a led connected to GPIO 2 every second.

Flash blink example using following commands:

```
# Flash the blink.avm to the device
$ $IDF_PATH/components/esptool_py/esptool/esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect  0x210000 build/examples/erlang/esp32/blink.avm
```
