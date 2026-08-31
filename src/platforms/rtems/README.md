<!---
  Copyright 2026 Peter M. <petermm@gmail.com>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM on RTEMS

This directory contains the RTEMS 6.2 port of AtomVM.

Supported bring-up targets:

- SPARC `erc32` on SIS (`rtems-run --rtems-bsps=erc32-sis`)
- ARM `imx7` on QEMU (`qemu-system-arm -M mcimx7d-sabre`)

Other RTEMS 6 BSPs can be selected at configure time once a matching toolchain
and BSP are installed. `imx7` is the NXP i.MX 7Dual BSP; QEMU models the
SABRE board, not GRiSP 2 (same SoC family, different pinmux and FDT).

This is a bring-up port: no SMP, no JIT, no sockets, and no filesystem loading
of `.avm` files. Applications are compiled on the host and embedded in the
RTEMS executable. Pack `atomvmlib-rtems.avm` (estdlib + eavmlib + `avm_rtems`)
together with the application so `init:boot/1` can start the entry module.

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

## CMake options

| Option | Default | Description |
| --- | --- | --- |
| `RTEMS_PREFIX` | (required) | RTEMS 6.2 installation prefix |
| `RTEMS_BSP` | `sparc/erc32` | Architecture/BSP (`arch/bsp`) |
| `RTEMS_VERSION` | `6` | Major toolchain version (`sparc-rtems6-gcc`) |
| `AVM_PACK` | empty | Host-built `.avm` to embed |
| `AVM_DISABLE_JIT` | `ON` | JIT is not supported yet |
| `AVM_DISABLE_SMP` | `ON` | Forced off for this port |
