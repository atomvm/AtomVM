<!---
  Copyright 2023 Davide Bettio <davide@uninstall.it>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM Update Instructions

## v0.6.0-alpha.0 -> v0.6.0-alpha.1

- **Libraries (or boot .avm file) from latest version must be used**. Standard library from
`v0.6.0-alpha.0` cannot work on top of latest version.
- Address (offset) of programs for Pico was changed from 0x100A0000 to 0x10100000. UF2
binaries need to be rebuilt with the proper offset using `uf2tool`.
- On ESP32, SSID and PSK stored in NVS are no longer read by network module. Applications
must fetch the values and pass them to `network:start/1` or `network:start_link/1`.
- The `lib.avm` partition is no longer supported on ESP32.  If you have been using a
spacialized partitioning of your ESP32 flash (uncommon), AtomVM will no longer try to load
code off this partition name.
