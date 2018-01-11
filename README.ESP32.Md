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
