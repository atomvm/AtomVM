<!--
 Copyright 2026 Peter M <petermm@gmail.com>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM Zephyr simulator tests

The Zephyr test app reuses the normal `src/platforms/zephyr` build through
`cmake/AtomVMZephyrApp.cmake`. The QEMU scenario enables `AVM_ZEPHYR_BOOT_TEST`
for a boot smoke test, while `native_sim` embeds a small Erlang test AVM and
uses Zephyr's I2C emulator to exercise the AtomVM Zephyr I2C NIFs.

Run the suite from the AtomVM repository root after initializing the Zephyr
west workspace:

```shell
west twister -T src/platforms/zephyr/tests -p qemu_x86_64 --inline-logs
```

Or use the Zephyr Docker image wrapper from `src/platforms/zephyr`:

```shell
./docker-test.sh
./docker-test.sh -b qemu_x86_64
./docker-test.sh -b native_sim
```

The `-b` option mirrors `docker-build.sh`. ESP32 scenarios are currently
build-only until a simulator or device harness is added.

Erlang-driven Zephyr tests follow the ESP32 and RP2 pattern: platform-specific
modules live under `test_erl_sources`, compile into a test AVM during CMake
configuration, and execute from the Zephyr test app.
