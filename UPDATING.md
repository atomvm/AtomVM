<!---
  Copyright 2023 Davide Bettio <davide@uninstall.it>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM Update Instructions

## v0.6.0-beta.1 -> v0.6.0-rc.0

- Drivers that send messages from Esp32 callbacks should use new functions
`port_send_message_from_task`, `globalcontext_send_message_from_task` or
`memory_destroy_heap_from_task` instead of `port_send_message`,
`globalcontext_send_message` and `memory_destroy_heap`.

## v0.6.0-alpha.2 -> v0.6.0-beta.0

- Registers are no longer preserved by GC by default when invoking nifs, as part of the fix
of interpretation of the emulator of the live parameter of many opcodes. NIFs may need
to call `memory_ensure_free_with_roots` and pass their arguments are roots, instead of
`memory_ensure_free` or `memory_ensure_free_opt`.
- Port call message tuple format has been changed, hence previous version of the standard library
  cannot be used. **Libraries (or boot .avm file) from latest version must be used**.

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
