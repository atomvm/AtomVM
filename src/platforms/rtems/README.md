<!---
  Copyright 2026 Peter M. <petermm@gmail.com>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM on RTEMS

This directory contains the RTEMS 6.2 port of AtomVM.

Supported bring-up targets:

- SPARC `erc32` on SIS (`rtems-run --rtems-bsps=erc32-sis`)
- ARM `realview_pbx_a9_qemu` on QEMU (`qemu-system-arm -M realview-pbx-a9`)
- ARM `imx7` on QEMU (`qemu-system-arm -M mcimx7d-sabre`)
- GRiSP 2 (`arm/imx7` with `RTEMS_BOARD=grisp2`; hardware validation pending)

Other RTEMS 6 BSPs can be selected at configure time once a matching toolchain
and BSP are installed. `imx7` is the NXP i.MX 7Dual BSP; QEMU models the
SABRE board, not GRiSP 2 (same BSP family, different pinmux, PHY, and FDT).
The GRiSP 2 build selects the board-specific LibBSD nexus and checks that the
bootloader supplied an FDT with the `embeddedbrains,grisp2` compatible string.

This is a bring-up port: no SMP or JIT. Applications are compiled on the host
and embedded in the RTEMS executable. GRiSP 2 can instead load `app.avm`
from its SD card, using the embedded application as a fallback.
Pack `atomvmlib-rtems.avm` (estdlib + eavmlib + `avm_network` +
`avm_rtems`) together with the application so `init:boot/1` can start the
entry module.

Sockets are provided on `arm/imx7` when `rtems-libbsd` is installed. Without
LibBSD, and on SPARC `erc32` and ARM `realview_pbx_a9_qemu`,
`atomvm_rtems:wait_dhcp/1` returns `{error, enotsup}`.

UART implements `uart_hal` over RTEMS Termios (`/dev/console`, erc32
`/dev/console_a` / `/dev/console_b`, imx7 `/dev/ttyS0` … `/dev/ttyS6`).
I2C implements `i2c_hal` with Linux-style `I2C_RDWR` on imx7
(`/dev/i2c-0`, registered from FDT alias `i2c0` on first open). SPARC erc32
and ARM RealView have no supported I2C controller; `i2c:init/1` returns
`{error, enotsup}`.
GPIO implements `gpio_hal` using the RTEMS i.MX GPIO driver on imx7. Pins can
be addressed as `{Bank, Pin}` (`1..7`, `0..31`) or through an FDT property.
GPIO on erc32 and RealView returns `{error, enotsup}`.

## Prerequisites

Install RTEMS 6.2 tools and the BSP you want to run. The official release
sources are at <https://ftp.rtems.org/pub/rtems/releases/6/6.2/>.

Example for SPARC / erc32:

```sh
export RTEMS_PREFIX=$HOME/development/rtems/6.2
wget https://ftp.rtems.org/pub/rtems/releases/6/6.2/sources/rtems-source-builder-6.2.tar.xz
tar xJf rtems-source-builder-6.2.tar.xz
cd rtems-source-builder-6.2/rtems
../source-builder/sb-set-builder --prefix="$RTEMS_PREFIX" 6/rtems-sparc
../source-builder/sb-set-builder --prefix="$RTEMS_PREFIX" \
    --target=sparc-rtems6 --with-rtems-bsp=sparc/erc32 --with-rtems-tests=no \
    6/rtems-kernel
export PATH="$RTEMS_PREFIX/bin:$PATH"
```

You also need a host AtomVM build (Erlang/OTP 28, CMake, gperf) to compile BEAM
and pack `.avm` files. OTP 25 can emit opcodes this VM no longer decodes by
default (`bs_test_tail2`); CI uses OTP 28.

## Build

From the repository root, build a host AtomVM and a boot AVM:

```sh
cmake -S . -B build-host -G Ninja
cmake --build build-host -t rtems_boot_test atomvmlib-rtems
mkdir -p src/platforms/rtems/build
./build-host/tools/packbeam/packbeam create -s test_boot \
    src/platforms/rtems/build/rtems_boot.avm \
    build-host/src/platforms/rtems/tests/test_erl_sources/test_boot.beam \
    build-host/libs/atomvmlib-rtems.avm
```

Cross-compile the RTEMS image, embedding that AVM:

```sh
cmake -S src/platforms/rtems -B src/platforms/rtems/build -G Ninja \
    -DCMAKE_TOOLCHAIN_FILE=cmake/rtems-toolchain.cmake \
    -DRTEMS_PREFIX="$RTEMS_PREFIX" \
    -DRTEMS_BSP=sparc/erc32 \
    -DRTEMS_VERSION=6 \
    -DAVM_PACK="$PWD/src/platforms/rtems/build/rtems_boot.avm"
cmake --build src/platforms/rtems/build
```

## Run

```sh
rtems-run --rtems-bsps=erc32-sis \
    src/platforms/rtems/build/AtomVM-erc32.exe
```

A successful boot of `rtems_boot_test` prints `{atomvm_rtems_boot,rtems}`
and `Return value: ok` before the RTEMS exit fatal. SIS then traps (`ta 0x0`);
that shutdown is expected. There must be no `Failed load module` line.

## ARM RealView PBX-A9 / QEMU

Install ARM tools and the `realview_pbx_a9_qemu` BSP:

```sh
../source-builder/sb-set-builder --prefix="$RTEMS_PREFIX" 6/rtems-arm
../source-builder/sb-set-builder --prefix="$RTEMS_PREFIX" \
    --target=arm-rtems6 \
    --with-rtems-bsp=arm/realview_pbx_a9_qemu \
    --with-rtems-tests=no \
    6/rtems-kernel
```

Cross-compile as above with `-DRTEMS_BSP=arm/realview_pbx_a9_qemu`. Run the
resulting ELF directly in QEMU; this BSP does not require a DTB or debugger
bootstrap:

```sh
qemu-system-arm -net none -nographic -no-reboot -M realview-pbx-a9 -m 256M \
    -kernel src/platforms/rtems/build/AtomVM-realview_pbx_a9_qemu.exe
```

`-no-reboot` makes QEMU exit when RTEMS resets the board during shutdown. The
console UART is supported. I2C, GPIO, and networking currently return
`{error, enotsup}` on this target.

## i.MX 7 / QEMU SABRE

Install ARM tools and the `imx7` BSP (separate prefix or the same `RTEMS_PREFIX`):

```sh
../source-builder/sb-set-builder --prefix="$RTEMS_PREFIX" 6/rtems-arm
../source-builder/sb-set-builder --prefix="$RTEMS_PREFIX" \
    --target=arm-rtems6 --with-rtems-bsp=arm/imx7 --with-rtems-tests=no \
    6/rtems-kernel
```

Cross-compile with `-DRTEMS_BSP=arm/imx7`. The ELF is `AtomVM-imx7.exe`.

The imx7 BSP does not initialize clocks or pins; it expects a bootloader FDT
pointer in ARM `r2`. QEMU `-kernel` of an ELF leaves `r2=0` and ignores `-dtb`,
so the helper raw-loads the DTB at `0xb0000000` (above the ELF workspace) and
sets `r2` via gdb.

Build Linux `imx7d-sdb.dtb` from the v6.1 DTS (do not enable armhf apt on
amd64 hosts). Sparse-checkout must include `include/uapi/linux` because
`linux-event-codes.h` is a symlink there. Patch the DTB for QEMU's 1 GiB
RAM and an 8 MHz ARM generic-timer frequency so RTEMS does not touch
unimplemented system-counter MMIO:

```sh
fdtput -t x imx7d-sdb-qemu.dtb /memory@80000000 reg 0x80000000 0x40000000
fdtput -t x imx7d-sdb-qemu.dtb /timer clock-frequency 0x7a1200
src/platforms/rtems/tools/run-imx7-qemu.sh \
    -d imx7d-sdb-qemu.dtb \
    src/platforms/rtems/build/AtomVM-imx7.exe
```

QEMU default RAM is 128 MiB, which is smaller than this ELF; the helper uses
`-m 1024M`. This is SoC-level UART bring-up, not GRiSP 2.

Open a second UART with `{peripheral, "/dev/ttyS1"}` (imx7) or
`"/dev/console_b"` (erc32). I2C on imx7:

```erlang
{ok, Bus} = i2c:init([{peripheral, "/dev/i2c-0"}, {fdt_alias, "i2c0"}]),
i2c:master_transmit(Bus, 16#50, <<16#00>>, 500).
```

GPIO direct bank/pin access requires the pin mux to have been configured by
the BSP or device tree:

```erlang
ok = gpio:set_pin_mode({1, 3}, output),
ok = gpio:digital_write({1, 3}, high).
```

An FDT consumer property can instead identify a pin and preserve its active
polarity:

```erlang
Pin = #{path => "/leds/status", property => "gpios", index => 0},
ok = gpio:set_pin_mode(Pin, output),
ok = gpio:digital_write(Pin, high).
```

GPIO pull configuration, open-drain mode, and interrupts are not yet
supported and return `{error, enotsup}`.

## Host startup regression test

On Linux, exercise the RTEMS boot fallback using the native VM and stubbed
board I/O:

```sh
cmake -S . -B build-host -G Ninja -DAVM_DISABLE_JIT=ON
cmake --build build-host -t test-rtems-startup
./build-host/tests/rtems-startup/test-rtems-startup
```

The test caps VM allocations at 256 KiB, forces failures late in module
loading and partway through import construction, and checks that the embedded
app starts and all VM allocations are released. It does not require an RTEMS
toolchain or board.

## Networking (imx7 / LibBSD)

Build LibBSD with the official RTEMS 6.2 Source Builder set. The `imx7` BSP
set uses `6/rtems-libbsd` (FreeBSD 12 `rtems-libbsd-6.2.tar.xz`), which is
internally consistent for i.MX7. The FreeBSD 14 6.2 tarball is not.

```sh
cd rtems-source-builder-6.2/rtems
../source-builder/sb-set-builder --prefix="$RTEMS_PREFIX" \
    --target=arm-rtems6 --with-rtems-bsp=arm/imx7 \
    6/rtems-libbsd
```

CMake links `libbsd.a` when it is present at
`$RTEMS_PREFIX/arm-rtems6/imx7/lib/libbsd.a`. The image initializes LibBSD,
brings up `lo0`, starts dhcpcd on `ffec0`, and installs the DHCP-provided name
servers atomically in `/etc/resolv.conf`. `atomvm_rtems:wait_resolver/1` waits
until that resolver configuration is installed. QEMU user networking attaches
to the onboard ENET with:

```sh
src/platforms/rtems/tools/run-imx7-qemu.sh \
    -d imx7d-sdb-qemu.dtb \
    -n user,model=imx.enet,hostfwd=tcp::8080-:8080 \
    src/platforms/rtems/build/AtomVM-imx7.exe
```

The Linux v6.1 `imx7d-sdb` DTB already describes `fsl,imx7d-fec`, PHY, MDIO,
interrupts, and clocks used by QEMU and the LibBSD `ffec` driver. Pack a
network test with `rtems_net_test` / `test_net.erl` to cover DHCP, DNS, outbound
TCP, a guest echo server on port 8080, and host-to-guest forwarding.

## GRiSP 2 hardware

For a prebuilt image, download the `atomvm-grisp2-sd-card-<commit>` artifact
from the RTEMS Build workflow. GitHub downloads it as a `.zip` file. Follow
the [SD-card installation guide](boards/grisp2/INSTALL.md), also included as
`README.md` at the top level of the ZIP. No RTEMS toolchain is needed to
install the prebuilt image.

GRiSP 2 uses an i.MX6ULL with RTEMS's shared `arm/imx7` BSP. Reuse the
RTEMS 6.2 ARM compiler, `imx7` BSP, and LibBSD from the QEMU build above;
LibBSD already includes the KSZ8091 PHY driver and i.MX nexus devices.
There is no additional AtomVM board driver or `libgrisp` dependency.
The GRiSP toolchain repository's RTEMS 5 prefix is not compatible with this
port, and building that entire repository is unnecessary.

A generated board DTB is bundled in
[`boards/grisp2`](boards/grisp2/README.md), with its source revision and
regeneration instructions. CMake validates its header and board compatible
string and copies it to `oftree` beside the image. Override it with
`-DRTEMS_DTB=/absolute/path/to/custom.dtb` for a different board
revision. The bootloader passes this DTB separately; it is not embedded in
the executable.

Install `u-boot-tools` and `device-tree-compiler` (`dtc` on Homebrew) for
image packaging and DTB validation.

Build and package a hardware smoke test as follows:

```sh
cmake -S . -B build-host -G Ninja
cmake --build build-host -t rtems_grisp2_test atomvmlib-rtems
mkdir -p src/platforms/rtems/build
./build-host/tools/packbeam/packbeam create -s test_grisp2 \
    src/platforms/rtems/build/rtems_grisp2.avm \
    build-host/src/platforms/rtems/tests/test_erl_sources/test_grisp2.beam \
    build-host/libs/atomvmlib-rtems.avm

cmake -S src/platforms/rtems -B src/platforms/rtems/build-grisp2 -G Ninja \
    -DCMAKE_TOOLCHAIN_FILE=cmake/rtems-toolchain.cmake \
    -DRTEMS_PREFIX="$RTEMS_PREFIX" \
    -DRTEMS_BSP=arm/imx7 \
    -DRTEMS_BOARD=grisp2 \
    -DAVM_PACK="$PWD/src/platforms/rtems/build/rtems_grisp2.avm"
cmake --build src/platforms/rtems/build-grisp2
```

`RTEMS_BOARD=grisp2` also enables creation of `AtomVM-grisp2.zImage`. The
image is converted to a flat binary, gzip-compressed, and wrapped with U-Boot
`mkimage` at load/entry address `0x80200000`. Inspect it with:

```sh
mkimage -l src/platforms/rtems/build-grisp2/AtomVM-grisp2.zImage
```

For the first boot, copy `AtomVM-grisp2.zImage` to the first FAT SD-card
partition as `zImage` and copy the build's `oftree` beside it. Copy
`src/platforms/rtems/build/rtems_grisp2.avm` as `app.avm` to exercise the SD
loader. Install `boards/grisp2/atomvm.conf` as `loader/entries/atomvm.conf`
on the same partition so barebox can find the image and device tree. See
the [installation guide](boards/grisp2/INSTALL.md) for serial console setup
and SD boot selection.

The smoke test prints `{atomvm_grisp2,rtems}`, `{grisp2,uart,ok}`,
`{grisp2,led,ok}`, `{grisp2,gpio,high}` or `{grisp2,gpio,low}`, and
`{grisp2,eeprom,ok}`, followed by `Return value: ok`. The first RGB LED's
red channel lights for half a second. The EEPROM check reads eight bytes
at I²C address `0x57` and does not write EEPROM contents.
Use `rtems_net_test` separately to verify `ffec0` DHCP/DNS/TCP on the
physical Ethernet link.

CI cross-builds the hardware image in the existing `arm/imx7` job and uploads
the `atomvm-grisp2-sd-card-<commit>` artifact. It contains ready-to-copy
`zImage`, `oftree` and `app.avm` files, a boot-loader entry, installation
instructions, source revision details, device-tree sources and licenses.
The debug ELF is named `debug/AtomVM-grisp2.elf` in the bundle. Physical
boot, UART, GPIO, I²C and Ethernet remain manual checks; a successful cross-build does not establish
that they work on a board.

### Updating the application on GRiSP 2

Install the firmware containing the SD loader once. Then applications can be
changed by replacing `app.avm` in the root of the existing SD-card FAT
partition and rebooting. No additional partition or firmware rebuild is
needed for application-only changes:

```text
zImage                      RTEMS + AtomVM + embedded fallback application
oftree                      Board device tree
app.avm                     Replaceable application, including its libraries
loader/entries/atomvm.conf   Boot image and device-tree selection
```

For example, from the repository root, build and pack `hello_world` using
the existing host build:

```sh
cmake --build build-host -t hello_world atomvmlib-rtems
./build-host/tools/packbeam/packbeam create -s hello_world \
    build-host/app.avm \
    build-host/examples/erlang/hello_world.beam \
    build-host/libs/atomvmlib-rtems.avm
```

With the board powered off, replace the card's `app.avm` with
`build-host/app.avm`, safely eject the card, and reboot. Keep `zImage`,
`oftree` and the boot-loader entry. A native driver or runtime change still
requires a new `zImage`.
The boot log identifies the selected file:

```text
AtomVM: Loaded /media/mmcsd-0-0/app.avm (... bytes)
Starting: hello_world.beam...
```

The media server starts before LibBSD attaches SD devices and mounts FAT
filesystems. The loader waits up to five seconds for the configured file,
then reads the complete pack into RAM (maximum 32 MiB). Missing files,
read/allocation failures, invalid pack or BEAM chunk structure, and startup
module load failures select the embedded application. The checks do not
validate all bytecode semantics: build the pack with the supported OTP and
libraries. An error after an application starts is reported normally; it
does not silently run the fallback. Remove `app.avm` to boot the embedded
hardware smoke test.

The default RTEMS path assumes the SD card is `mmcsd-0` and uses its first
partition, `/media/mmcsd-0-0`. If device numbering or the card layout differs,
set `-DRTEMS_APP_PATH=/media/your-volume/app.avm` when building the
firmware. For an unpartitioned FAT volume on `mmcsd-0`, use
`/media/mmcsd-0/app.avm`. The loader selects this path explicitly; it does
not infer which volume barebox booted.

The CI artifact includes the smoke-test pack as `app.avm`. On hardware,
check a valid replacement, a missing file, and a truncated copy: the latter
two must print the fallback message and run the embedded example.

## CMake options

| Option | Default | Description |
| --- | --- | --- |
| `RTEMS_PREFIX` | (required) | RTEMS 6.2 installation prefix |
| `RTEMS_BSP` | `sparc/erc32` | Architecture/BSP (`arch/bsp`) |
| `RTEMS_BOARD` | `generic` (`qemu` for `arm/imx7`) | Board integration (`generic`, `qemu`, or `grisp2`) |
| `RTEMS_VERSION` | `6` | Major toolchain version (`sparc-rtems6-gcc`) |
| `AVM_PACK` | empty | Host-built `.avm` to embed |
| `RTEMS_CREATE_ZIMAGE` | `ON` for `grisp2` | Create a GRiSP 2 bootloader-ready image |
| `RTEMS_IMAGE_LOAD_ADDRESS` | `0x80200000` | GRiSP 2 image load and entry address |
| `RTEMS_DTB` | bundled `boards/grisp2/imx6ul-grisp2.dtb` | Board DTB to validate and copy as `oftree` when packaging |
| `RTEMS_APP_PATH` | `/media/mmcsd-0-0/app.avm` | RTEMS filesystem path for the replaceable application (GRiSP 2 only) |
| `AVM_DISABLE_JIT` | `ON` | JIT is not supported yet |
| `AVM_DISABLE_SMP` | `ON` | Forced off for this port |
