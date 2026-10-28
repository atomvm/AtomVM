<!--
Copyright 2026 Peter M. <petermm@gmail.com>
SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Generic GPIO API for RTEMS 6

`gpio.c` and `include/dev/gpio/gpio.h` are unmodified copies of
`cpukit/dev/gpio/gpio.c` and `cpukit/include/dev/gpio/gpio.h` from RTEMS commit
[`402de13a864f8876c48a8c108cc2e3541d87bd07`](https://github.com/RTEMS/rtems/commit/402de13a864f8876c48a8c108cc2e3541d87bd07),
the kernel revision pinned in `../docker/dependencies.sh`.

The i.MX/GRiSP 2 build uses this upstream implementation only when its SDK
lacks `<dev/gpio/gpio.h>`. This lets RTEMS 6 use the same device API without
changing its compiler, BSP or LibBSD version. SDKs that provide the API use
their own implementation instead. The upstream BSD-2-Clause notices are
preserved in both files.
