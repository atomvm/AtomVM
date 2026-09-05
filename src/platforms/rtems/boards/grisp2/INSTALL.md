<!---
  Copyright 2026 Peter M. <petermm@gmail.com>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Install AtomVM on GRiSP 2

The RTEMS Build workflow provides `atomvm-grisp2-sd-card-<commit>.zip`.
Extract the ZIP and copy its boot files to an SD card. This is a file
bundle, not a raw disk image for `dd` or a flashing tool. The board uses
its existing barebox bootloader.

This image contains the hardware smoke test both as the replaceable
`app.avm` and as an embedded fallback. Physical board validation is still
pending; the CI build verifies cross-compilation and packaging.

## Bundle contents

| File | Purpose |
| --- | --- |
| `zImage` | RTEMS + AtomVM + embedded smoke test, in U-Boot image format |
| `oftree` | GRiSP 2 device tree, passed separately by barebox |
| `app.avm` | Replaceable smoke-test application and its libraries |
| `loader/entries/atomvm.conf` | Boot-loader entry pointing to `zImage` and `oftree` |
| `README.md` | These installation instructions |
| `BUILD.txt` | Exact source revision and CI run URLs |
| `debug/AtomVM-grisp2.elf` | ELF executable for GDB and debugging |
| `dtb-source/` | Device-tree sources, provenance, and regeneration instructions |
| `LICENSES/`, `oftree.license` | License texts and device-tree notices |

Keep the boot filenames `zImage`, `oftree` and `app.avm` exactly as shown.
The debug ELF is not needed on the card.

## First boot from SD

1. With the board powered off, mount the SD card on your computer. Use its
   first partition, formatted as FAT32. An existing GRiSP SD card can be
   reused; no extra application partition is needed. Save any existing boot
   files before replacing them.
2. Copy `zImage`, `oftree` and `app.avm` from the extracted bundle to the
   root of that partition, and install `loader/entries/atomvm.conf` at the
   same relative path. For example, from the extracted directory on
   macOS, with the card mounted at `/Volumes/GRISP`:

   ```sh
   SD_CARD=/Volumes/GRISP
   cp zImage oftree app.avm "$SD_CARD/"
   mkdir -p "$SD_CARD/loader/entries"
   cp loader/entries/atomvm.conf "$SD_CARD/loader/entries/"
   sync
   ```

   On Linux, set `SD_CARD` to the mounted FAT partition's directory.
   The card root should contain the three files and the `loader` directory
   directly, without a surrounding `build-grisp2` or bundle directory.
3. Safely eject the card and insert it into the powered-off GRiSP 2.
   Connect the Micro-B USB console at **115200 baud, 8N1**, with flow
   control disabled. Use the board's second USB serial interface; see the
   [GRiSP serial guide](https://github.com/grisp/grisp/wiki/Connecting-over-Serial)
   for identifying the device. A Linux example is:

   ```sh
   picocom --baud 115200 --flow n /dev/ttyUSB1
   ```

4. Power on or reset the board and watch the boot log. If the existing eMMC
   application boots instead, interrupt the barebox countdown and run
   `boot sdcard` with the
   [stock GRiSP environment](https://github.com/grisp/grisp2-rtems-toolchain/blob/8d9f44ac3296a0378c35f83a946f17cf5000536c/barebox/env/boot/sdcard).
   If the card has other boot entries, use `boot -m mmc0` and select
   **AtomVM on GRiSP 2**. Keep the existing bootloader and eMMC installation
   during this first SD test.

Bootloader environments can differ. The supplied boot-loader entry loads
both `zImage` and `oftree` from the card. At a barebox prompt, `ls /env/boot`
shows the available boot entries; `boot <entry>` runs the selected entry. To boot
directly from an already mounted card, use
`bootm -o <sd-mount>/oftree <sd-mount>/zImage`, replacing `<sd-mount>` with
the card's actual barebox mount path. See the
[barebox boot documentation](https://www.barebox.org/doc/latest/user/booting-linux.html).

## Verify the smoke test

The serial log should include `AtomVM: Loaded /media/mmcsd-0-0/app.avm`,
followed by `Starting: test_grisp2.beam...` and these markers:

```text
{atomvm_grisp2,rtems}
{grisp2,uart,ok}
{grisp2,led,ok}
{grisp2,gpio,high} or {grisp2,gpio,low}
{grisp2,eeprom,ok}
Return value: ok
```

The first RGB LED's red channel lights for half a second. The EEPROM test
reads eight bytes at I2C address `0x57`. This smoke test returns after its
checks; it does not provide an interactive Erlang shell or test Ethernet.

If AtomVM reports that it is using the embedded application, the firmware
booted but the SD application was not loaded. Check the filename, the FAT
partition, and the log's error. This build looks for `app.avm` at
`/media/mmcsd-0-0/app.avm`, on the first partition of RTEMS device `mmcsd-0`.
Different device numbering or an unpartitioned FAT volume requires building
with a matching `RTEMS_APP_PATH`.

## Replace the application

For application-only updates, replace `app.avm` in the card root and reboot.
Keep `zImage`, `oftree` and the boot-loader entry. Include the application's
dependencies in the AVM pack; CI uses Erlang/OTP 28 and `atomvmlib-rtems.avm`.

For example, from the AtomVM source repository at the bundle's revision:

```sh
cmake -S . -B build-host -G Ninja
cmake --build build-host -t hello_world atomvmlib-rtems
./build-host/tools/packbeam/packbeam create -s hello_world \
    build-host/app.avm \
    build-host/examples/erlang/hello_world.beam \
    build-host/libs/atomvmlib-rtems.avm
```

Power off the board, replace the card's `app.avm` with `build-host/app.avm`,
eject the card and reboot. The log should now show
`Starting: hello_world.beam...`. Removing `app.avm` selects the embedded
smoke test. Runtime or native-driver changes require a new `zImage`.

## Older CI artifacts

The older `atomvm-grisp2.zip` layout puts boot files in `build-grisp2/`.
Copy `build-grisp2/AtomVM-grisp2.zImage` to the card as `zImage`, and copy
`build-grisp2/oftree` and `build-grisp2/app.avm` beside it. Also create
`loader/entries/atomvm.conf` on the card with:

```text
title AtomVM on GRiSP 2
linux /zImage
devicetree /oftree
```

Then follow the serial console and SD boot steps above. The older bundle's
`AtomVM-grisp2.exe` is an ELF debug executable; use the packaged boot image
for SD boot.
