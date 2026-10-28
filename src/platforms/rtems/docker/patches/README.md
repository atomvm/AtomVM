<!--
Copyright 2026 Peter M. <petermm@gmail.com>

SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# RTEMS dependency patches

Each directory is an ordered series for a separate upstream repository. Apply
all patches in filename order to the revision pinned in
[`../dependencies.sh`](../dependencies.sh), before building. Docker and native
build scripts do this automatically. The kernel series is applied as a unit;
its intermediate patches are topical layers, not standalone board ports.

## Kernel: `rtems/`

Upstream: <https://github.com/RTEMS/rtems.git>, `RTEMS_KERNEL7_COMMIT`.

1. Shared RISC-V trap vectors, exception state and SMP prerequisites.
2. ESP32 board definitions, public BSP interface, direct-boot memory maps and
   ROM/SRAM reservations for C3/C5/C6/H2/S31 and both P4 revisions.
3. GPIO and shared pin arbitration.
4. I2C driver.
5. UART driver.
6. GPSPI2 driver.
7. Bluetooth block power control.
8. Interrupt routing, CLIC/PLIC state, Bluetooth/Wi-Fi routes, SMP IPIs,
   systimer ticks and the emulator IRQ fallback.
9. Startup, watchdogs, SRAM/PMA permissions, PLLs, calibrated voltage,
   RTC clocks, P4 PVT, secondary CPUs and completed driver registration.

The driver and startup patches include their chip-specific corrections rather
than carrying later fixup patches. Source copyright and license notices are
unchanged; mail headers credit the original authors and identify legacy patch
numbers where a topic draws from several earlier patches.

## Network ports: `rtems-lwip/`

Upstream: <https://github.com/RTEMS/rtems-lwip.git>, `RTEMS_LWIP_COMMIT`.

The first five patches add C3, C6, C5, S31 and P4 ports, respectively. The C3
port includes its SRAM-sized pbuf pool and heap limits. The final patch handles
DHCP behind Wi-Fi AP proxy ARP for C3/C5/C6/S31; it does not change P4.

## Radio adapter: `rtems-esp-wifi/`

Upstream: <https://github.com/thesamprice/rtems-esp-wifi.git>,
`RTEMS_ESP_WIFI_COMMIT`.

The ordered topics cover chip ports, interrupt locking/routing, IDF-compatible
PHY lifecycle and software coexistence, shared Bluetooth/PHY ownership, sleep
clock calibration and the C5 Bluetooth adapter.

## Regeneration

Regenerate patches with `git format-patch` from topical commits on the pinned
dependency. Do not hand-edit generated diffs. When consolidating or reordering,
apply the old and new series separately to the same baseline and compare
`git write-tree` results: they must be identical, including file modes. Build
the complete new series before accepting a packaging-only change.
