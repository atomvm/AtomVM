<!---
  Copyright 2026 Peter M. <petermm@gmail.com>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# GRiSP 2 device tree

`imx6ul-grisp2.dtb` is generated without board changes from
[`grisp/grisp2-rtems-toolchain` at
`8d9f44ac3296a0378c35f83a946f17cf5000536c`](https://github.com/grisp/grisp2-rtems-toolchain/tree/8d9f44ac3296a0378c35f83a946f17cf5000536c).
All input DTS/DTSI files, binding headers and build scripts are included in
`grisp2-dtb-source.tar.gz`, exported from that commit. No submodules or cross
compiler are needed to regenerate the DTB. This pin is for the device tree;
AtomVM uses the RTEMS 6.2 compiler/BSP/LibBSD, not that repository's RTEMS 5
build configuration.

The source identifies the board as `GRiSP2` with root compatible strings
`embeddedbrains,grisp2`, `phytec,imx6ul-pcl063-emmc`, and `fsl,imx6ull`.
It describes the GRiSP 2 i.MX6ULL board layout at the pinned revision; it
does not identify a PCB revision. Hardware validation and confirmation of
the user's board revision are still pending. GRiSP 1 and Nano are not
covered. Use `RTEMS_DTB` for a board-specific replacement.

Regenerate with a host C preprocessor and `dtc` (the bundled binary was
generated with Apple Clang 21 and DTC 1.8.1):

```sh
tar xf grisp2-dtb-source.tar.gz
make -C grisp2-dtb-source/fdt CPP='cc -E'
cmp imx6ul-grisp2.dtb grisp2-dtb-source/fdt/b-dtb/imx6ul-grisp2.dtb
```

The upstream script preprocesses `imx6ul-grisp2.dts` using the bundled
include tree and runs `dtc -@ -O dtb -b 0 -p 1024`. Existing upstream DTC
warnings are expected. Other DTC versions may serialize the same tree
differently. SHA-256 of the bundled DTB:

```text
254450e9fbbab794cca3d0f9ecc5f0b12025f04a8903eea052cb176ad06e3af8
```

From the AtomVM repository root, validate the file without a cross compiler:

```sh
cmake -DRTEMS_DTB="$PWD/src/platforms/rtems/boards/grisp2/imx6ul-grisp2.dtb" \
    -DRTEMS_BOARD_FDT_COMPATIBLE=embeddedbrains,grisp2 \
    -P src/platforms/rtems/cmake/validate_dtb.cmake
```

The GRiSP board DTS is dual GPL-2.0-or-later/BSD-2-Clause, copyright 2020
embedded brains GmbH. Included SoC and module sources carry GPL-2.0-only
terms, including copyrights of Freescale Semiconductor and PHYTEC
Messtechnik. The generated DTB is distributed under GPL-2.0-only; see
`COPYING`. Original notices and binding-header license alternatives remain
in the source archive. Keep this metadata, license, and corresponding
source archive with redistributed DTBs.

The accompanying `.license` files provide machine-readable copyright and
licensing notices for the DTB and source archive. Canonical license texts
are in the repository's `LICENSES` directory.
