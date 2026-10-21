#!/usr/bin/env bash
#
# This file is part of AtomVM.
#
# Copyright 2026 Peter M. <petermm@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

set -euo pipefail

IMAGE_NAME="atomvm-rtems7-esp32:latest"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"

DO_SIMULATE=0 # 0: build only, 1: esp-emulator, 2: Wokwi
DOCKER_PLATFORM="${DOCKER_PLATFORM:-}"
TEST_MODULE="test_boot"
TEST_WIFI=0

usage() {
    cat <<EOF
Usage: $(basename "$0") [options]

Builds AtomVM for RTEMS 7 ESP32-C5 inside an isolated Docker container.

Options:
  --simulate          Run the boot test in esp-emulator after compiling
  --simulate-wokwi    Run the test with wokwi-cli (requires WOKWI_CLI_TOKEN)
  --test-wifi         Embed and run the Wi-Fi test (test_esp32c5_wifi)
  --platform <arch>   Target Docker platform (e.g., linux/arm64, linux/amd64)
  -h, --help          Show this message

EOF
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --simulate-wokwi)
            DO_SIMULATE=2
            shift
            ;;
        --simulate|--test)
            DO_SIMULATE=1
            shift
            ;;
        --test-wifi)
            TEST_MODULE="test_esp32c5_wifi"
            TEST_WIFI=1
            shift
            ;;
        --platform)
            DOCKER_PLATFORM="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        *)
            echo "Unknown option: $1" >&2
            usage
            ;;
    esac
done

if [ "${DO_SIMULATE}" -eq 2 ]; then
    if [ "${TEST_WIFI}" -eq 1 ]; then
        TEST_MODULE="test_wokwi_wifi"
    fi
    "${SCRIPT_DIR}/run-wokwi.sh" --check
fi

command -v docker >/dev/null 2>&1 || { echo "Error: docker is required" >&2; exit 1; }

echo "==> Compiling AtomVM for ESP32-C5 under RTEMS 7 inside Docker..."
RUN_CMD=(docker run)
if [ -n "${DOCKER_PLATFORM}" ]; then
    RUN_CMD+=(--platform "${DOCKER_PLATFORM}")
fi
RUN_CMD+=(--rm -v "${REPO_ROOT}:/workspace" -w /workspace "${IMAGE_NAME}" bash -c '
    set -euo pipefail

    if ! command -v rebar3 >/dev/null 2>&1; then
        echo "==> Ensuring rebar3 and Erlang dependencies are available..."
        apt-get update -qq && apt-get install -y -qq zlib1g-dev >/dev/null 2>&1 || true
        . /workspace/src/platforms/rtems/docker/dependencies.sh
        rtems_install_otp
    fi

    if [ -f build-host/CMakeCache.txt ] && grep -q "REBAR3-NOTFOUND" build-host/CMakeCache.txt; then
        rm -rf build-host/CMakeCache.txt build-host/CMakeFiles
    fi

    TEST_MOD="'"${TEST_MODULE}"'"
    echo "==> Building host AVM pack (${TEST_MOD})..."
    cmake -S . -B build-host -G Ninja -DAVM_DISABLE_JIT=ON
    cmake --build build-host -t rtems_boot_test rtems_esp32c5_wifi_test rtems_wokwi_wifi_test atomvmlib-rtems
    mkdir -p src/platforms/rtems/build-esp32c5
    ./build-host/tools/packbeam/packbeam create -s "${TEST_MOD}" \
        src/platforms/rtems/build-esp32c5/rtems_boot.avm \
        "build-host/src/platforms/rtems/tests/test_erl_sources/${TEST_MOD}.beam" \
        build-host/libs/atomvmlib-rtems.avm

    echo "==> Cross-compiling AtomVM for riscv/esp32c5db..."
    cmake -S src/platforms/rtems -B src/platforms/rtems/build-esp32c5 -G Ninja \
        -DCMAKE_TOOLCHAIN_FILE=cmake/rtems-toolchain.cmake \
        -DRTEMS_PREFIX=/opt/rtems/7 \
        -DRTEMS_BSP=riscv/esp32c5db \
        -DRTEMS_BOARD=esp32c5 \
        -DRTEMS_VERSION=7 \
        -DAVM_PACK=/workspace/src/platforms/rtems/build-esp32c5/rtems_boot.avm
    cmake --build src/platforms/rtems/build-esp32c5
')
"${RUN_CMD[@]}"

echo "==> Build successful! Direct-boot image is ready at:"
echo "    src/platforms/rtems/build-esp32c5/AtomVM-esp32c5db.bin"

if [ "${DO_SIMULATE}" -eq 2 ]; then
    "${SCRIPT_DIR}/run-wokwi.sh" esp32c5 "${REPO_ROOT}/src/platforms/rtems/build-esp32c5"
elif [ "${DO_SIMULATE}" -eq 1 ]; then
    echo "==> Running simulation with esp-emulator..."
    if command -v esp-emu >/dev/null 2>&1; then
        HTTP_PID=""
        SNTP_PID=""
        if [ "${TEST_WIFI}" -eq 1 ]; then
            python3 -m http.server 8080 --directory "${REPO_ROOT}" >/dev/null 2>&1 &
            HTTP_PID=$!
            python3 "${REPO_ROOT}/src/platforms/rtems/tools/mock_sntp_server.py" 12300 > /tmp/mock_sntp.log 2>&1 &
            SNTP_PID=$!
        fi
        EMU_CMD=(esp-emu --chip esp32c5 --firmware "${REPO_ROOT}/src/platforms/rtems/build-esp32c5/AtomVM-esp32c5db.bin")
        if [ "${TEST_WIFI}" -eq 1 ]; then
            EMU_CMD+=(--batch-size 5000 --wifi-ssid "AtomVM-TestAP" --wifi-password "secretpassword" --net user)
        fi
        EMU_CMD+=(--timeout 60s --exit-on "Return value: ok")
        "${EMU_CMD[@]}" || true
        if [ -n "${HTTP_PID}" ]; then
            kill "${HTTP_PID}" 2>/dev/null || true
        fi
        if [ -n "${SNTP_PID}" ]; then
            kill "${SNTP_PID}" 2>/dev/null || true
        fi
    else
        SIM_CMD=(docker run)
        if [ -n "${DOCKER_PLATFORM}" ]; then
            SIM_CMD+=(--platform "${DOCKER_PLATFORM}")
        fi
        if [ "${TEST_WIFI}" -eq 1 ]; then
            SIM_CMD+=(--rm -v "${REPO_ROOT}:/workspace" "${IMAGE_NAME}" bash -c '
                python3 -m http.server 8080 --directory /workspace &
                HTTP_PID=$!
                python3 /workspace/src/platforms/rtems/tools/mock_sntp_server.py 12300 > /tmp/mock_sntp.log 2>&1 &
                SNTP_PID=$!
                trap "kill $HTTP_PID $SNTP_PID 2>/dev/null || true" EXIT
                esp-emu --chip esp32c5 \
                    --firmware /workspace/src/platforms/rtems/build-esp32c5/AtomVM-esp32c5db.bin \
                    --wifi-ssid "AtomVM-TestAP" --wifi-password "secretpassword" --net user --batch-size 5000 \
                    --timeout 60s --exit-on "Return value: ok"
            ')
        else
            SIM_CMD+=(--rm -v "${REPO_ROOT}:/workspace" "${IMAGE_NAME}" \
                esp-emu --chip esp32c5 \
                --firmware /workspace/src/platforms/rtems/build-esp32c5/AtomVM-esp32c5db.bin \
                --timeout 30s --exit-on "Return value: ok")
        fi
        "${SIM_CMD[@]}"
    fi
fi
