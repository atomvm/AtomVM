<!--
Copyright 2026 Peter M. <petermm@gmail.com>

SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# RTEMS & RTEMS-lwIP Patches

Patches applied on top of official upstream repositories:
- `rtems/`: Applied to the upstream RTEMS kernel (`https://gitlab.rtems.org/rtems/rtos/rtems.git`).
  - Device drivers: GPIO (`/dev/gpio-0`), I2C (`/dev/i2c-0`), UART (`/dev/ttyS0`), GPSPI2 (`/dev/spi-0`), pin arbitration.
  - Subsystems: Wi-Fi RAM execution region (`.wifi*iram`), peripheral interrupt 0, timer frequency correction, ROM boundary.
  - Emulation: IRQ assertion fallback for `esp-emulator`.
- `rtems-lwip/`: Applied to upstream RTEMS lwIP (`https://gitlab.rtems.org/rtems/pkg/rtems-lwip.git`).
  - Board definition: `riscv/esp32c3db.json`, `lwipbspopts.h`, decoupled `netstart.c`.
  - Sizing tuning: SRAM-efficient pbuf pool and heap sizes.

When building the Docker image or running `docker-build-esp32c3.sh`, any patches
in these directories are automatically applied to the respective source trees.
