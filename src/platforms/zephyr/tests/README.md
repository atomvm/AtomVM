<!--
 Copyright 2026 Peter M <petermm@gmail.com>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM Zephyr simulator tests

The Zephyr test app reuses the normal `src/platforms/zephyr` build through
`cmake/AtomVMZephyrApp.cmake`, and enables `AVM_ZEPHYR_BOOT_TEST` for a
QEMU-backed boot smoke test.

Run the suite from the AtomVM repository root after initializing the Zephyr
west workspace:

```shell
west twister -T src/platforms/zephyr/tests -p qemu_x86_64 --inline-logs
```

Or use the Zephyr Docker image wrapper from `src/platforms/zephyr`:

```shell
./docker-test.sh
./docker-test.sh -b qemu_x86_64
```

The `-b` option mirrors `docker-build.sh`, but the initial simulator suite is
run only on `qemu_x86_64`. ESP32 scenarios are currently build-only until a
simulator or device harness is added.

Future Erlang-driven Zephyr tests should follow the ESP32 and RP2 pattern:
place platform-specific Erlang modules under this test directory, compile them
into a test AVM during CMake configuration, and execute them from a Zephyr test
entry point or boot-test mode.
