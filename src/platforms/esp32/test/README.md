<!--
 Copyright 2024 Davide Bettio <davide@uninstall.it>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM device tests

AtomVM provides two paths for testing on device, the locally run QEMU emulator or the remote Wokwi CI.

# QEMU testing

Instructions for running the tests [on QEMU are documented here](https://www.atomvm.net/doc/main/build-instructions.html#running-tests-for-esp32).

# Wokwi CI testing

Wokwi CI is a commercial cloud CI, see [wokwi-ci/getting-started](https://docs.wokwi.com/wokwi-ci/getting-started), running it locally requires you to obtain a `WOKWI_CLI_TOKEN` and usage fees may apply - AtomVM uses it through the [pytest-embedded-wokwi](https://github.com/espressif/pytest-embedded/tree/main/pytest-embedded-wokwi) integration.

## Github CI/Actions

The `WOKWI_CLI_TOKEN` needs to be set in your `Repository secrets` Settings -> Actions secrets and variables.

## Installing prerequisites

1. The Wokwi CLI needs to be installed:

   ```shell
   curl -L https://wokwi.com/ci/install.sh | sh
   ```

   Or [alternative installation methods here](https://docs.wokwi.com/wokwi-ci/getting-started#cli-installation).

2. `WOKWI_CLI_TOKEN` needs to be set in your enviroment variables:

   ```shell
   export WOKWI_CLI_TOKEN="your-api-key"
   ```

3. A recent pytest, and pytest-embedded must be installed:

   ```shell
   $ pip install pytest==8.0.2 \
   pytest-embedded==1.8.1 \
   pytest-embedded-serial-esp==1.8.1 \
   pytest-embedded-idf==1.8.1 \
   pytest-embedded-wokwi==1.8.1
   ```

4. The ESP-IDF build environment must be installed and available:

   ```shell
   $ get_idf
   ```

## Running Wokwi CI

1. We need to use a special sdkconfig different from the QEMU one:
   ```shell
   $ cp sdkconfig.simtest-defaults sdkconfig.defaults
   ```
2. Set `IDF_TARGET`, and run `idf.py set-target ${IDF_TARGET}`:
   ```shell
   $ export IDF_TARGET=esp32 && idf.py set-target ${IDF_TARGET}
   ```
3. Wokwi CI uses a `diagram.json`, to describe the device used (specific board, pin connections, sensors, sd card etc). When changing `IDF_TARGET`, we need to clean it out:

   ```shell
   # only when changing IDF_TARGET
   $ rm diagram.json
   ```

   _NB!_ - on the `esp32` target, the SD card tests are enabled, so we need to use a special diagram.json, where a SD card is wired up:

   ```shell
   # when using esp32 target
   $ cp diagram_esp32.json diagram.json
   ```

   For all other targets we run without diagram.json.

4. Now we run `idf.py build` and run the CI:

   ```shell
   $ idf.py build && pytest -k 'test_atomvm_sim' --embedded-services=idf,wokwi     --wokwi-timeout=90000 --target=${IDF_TARGET} -s -W     ignore::DeprecationWarning
   ```
