Building AtomVM for ESP32
=========================

* build:

```
cd src/platforms/esp32/
make
```

* flash:
```
make flash
```

Running BEAM code on ESP32
==========================

* BEAM module must be stripped before using PackBEAM:

```
./PackBEAM packed.avm module.beam
```

* Packed BEAM module should be written to dedicated BEAM partition:

```
$IDF_PATH/components/esptool_py/esptool/esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect  0x110000 packed.avm
```

Running Hello World
===================

This example will print a Hello World message.

```
$ cd examples/erlang

# Compile the hello world
$ erlc hello_world.erl

# Pack it (the path to PackBEAM needs to be adjusted)
$ path/to/PackBEAM hello_world.avm hello_world.beam

# Flash it to the device
$ $IDF_PATH/components/esptool_py/esptool/esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect  0x110000 hello_world.avm

$ cd ../../src/platforms/esp32

# Open the serial monitor
$ make monitor
```

Running Blink Example
=====================

This example will switch on and off a led connected to GPIO 2 every second.

Compile and flash blink example using following commands:

```
$ cd examples/erlang/esp32

# Compile blink example
$ erlc blink.erl

# Pack it (the path to PackBEAM needs to be adjusted)
$ path/to/PackBEAM blink.avm blink.beam

# Flash it to the device
$ $IDF_PATH/components/esptool_py/esptool/esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect  0x110000 blink.avm
```
