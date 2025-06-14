<!--
 Copyright 2024 Davide Bettio <davide@uninstall.it>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM emulator/simulator tests

AtomVM provides two paths for testing "on device", the locally run QEMU emulator or the remote Wokwi CI simulator.

# QEMU emulator testing

Instructions for running the tests [on QEMU are documented here](https://doc.atomvm.org/main/build-instructions.html#running-tests-for-esp32).

# Wokwi CI simulator testing

Wokwi CI is a commercial cloud CI, see [wokwi-ci/getting-started](https://docs.wokwi.com/wokwi-ci/getting-started), running it locally requires you to obtain a `WOKWI_CLI_TOKEN` [Get token](https://wokwi.com/dashboard/ci) and usage fees may apply in the future - AtomVM uses it through the [pytest-embedded-wokwi](https://github.com/espressif/pytest-embedded/tree/main/pytest-embedded-wokwi) integration.

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
   $ pip install pytest==8.3.3 \
   pytest-embedded==1.11.5 \
   pytest-embedded-serial-esp==1.11.5 \
   pytest-embedded-idf==1.11.5 \
   pytest-embedded-wokwi==1.11.5
   ```

4. The ESP-IDF build environment must be installed and available:

   ```shell
   get_idf
   ```

## Compiling for and running Wokwi CI

1. We need to use a special sdkconfig (`sdkconfig.ci.wokwi`) different from the QEMU one. So we set `IDF_TARGET`, and run `idf.py -DSDKCONFIG_DEFAULTS=sdkconfig.ci.wokwi set-target ${IDF_TARGET}`:

   ```shell
   export IDF_TARGET=esp32 && idf.py -DSDKCONFIG_DEFAULTS=sdkconfig.ci.wokwi set-target ${IDF_TARGET}
   ```

2. Wokwi CI uses a `diagram.json`, to describe the device used (specific board, pin connections, sensors, sd card etc). 'diagram.json' files are available for each target in the sim_boards folder.

3. Now we run `idf.py build` and run the CI:

   ```shell
   idf.py build -DSDKCONFIG_DEFAULTS='sdkconfig.ci.wokwi' && pytest --embedded-services=idf,wokwi --wokwi-timeout=90000 --target=${IDF_TARGET} --wokwi-diagram=sim_boards/diagram.${IDF_TARGET}.json -s -W ignore::DeprecationWarning
   ```
