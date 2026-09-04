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

Other RTEMS 6 BSPs can be selected at configure time once a matching toolchain
and BSP are installed. `imx7` is the NXP i.MX 7Dual BSP; QEMU models the
SABRE board, not GRiSP 2 (same SoC family, different pinmux and FDT).

This is a bring-up port: no SMP, no JIT, and no filesystem loading of `.avm`
files. Applications are compiled on the host and embedded in the RTEMS
executable. Pack `atomvmlib-rtems.avm` (estdlib + eavmlib + `avm_network` +
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

## CMake options

| Option | Default | Description |
| --- | --- | --- |
| `RTEMS_PREFIX` | (required) | RTEMS 6.2 installation prefix |
| `RTEMS_BSP` | `sparc/erc32` | Architecture/BSP (`arch/bsp`) |
| `RTEMS_VERSION` | `6` | Major toolchain version (`sparc-rtems6-gcc`) |
| `AVM_PACK` | empty | Host-built `.avm` to embed |
| `AVM_DISABLE_JIT` | `ON` | JIT is not supported yet |
| `AVM_DISABLE_SMP` | `ON` | Forced off for this port |
