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

The `-b` option mirrors `docker-build.sh`. Wokwi runtime coverage is available
for the ESP32, ESP32-C3, and ESP32-S3 test builds:

```shell
./wokwi-test.sh -b esp32_devkitc/esp32/procpu
./wokwi-test.sh -b esp32c3_devkitm/esp32c3
./wokwi-test.sh -b esp32s3_devkitc/esp32s3/procpu
```

Normal Wokwi helper runs use a pristine Zephyr build so changes to Erlang
libraries embedded by the `HostAtomVM` external project cannot be hidden by a
stale build stamp. Use `--skip-build` only when intentionally rerunning the
already-built firmware.

The checked-in Wokwi diagrams are under `sim_boards`. Other ESP32 scenarios
remain build-only until Zephyr board support and a simulator harness are added.

The ESP32 Wokwi diagrams connect a slide potentiometer to each board's ADC
test channel. Zephyr selects that channel through the `zephyr,user`
`io-channels` devicetree property; the `adc` Erlang module exposes `open/0`,
`read/1,2`, and `close/1` and returns both the raw value and millivolts.

Erlang-driven Zephyr tests follow the ESP32 and RP2 pattern: platform-specific
modules live under `test_erl_sources`, compile into a test AVM during CMake
configuration, and execute from the Zephyr test app.
