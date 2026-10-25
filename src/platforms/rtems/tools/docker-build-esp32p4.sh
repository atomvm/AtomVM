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

DO_SIMULATE=0 # 0: build only, 1: esp-emulator
DOCKER_PLATFORM="${DOCKER_PLATFORM:-}"
TEST_MODULE="test_boot"

usage() {
    cat <<EOF
Usage: $(basename "$0") [options]

Builds AtomVM for RTEMS 7 ESP32-P4 revision 3.x inside a Docker container.
Rebuild atomvm-rtems7-esp32:latest after changing the BSP patches.

Options:
  --simulate          Run the boot test in esp-emulator after compiling
  --test-smp          Embed and run the SMP test
  --platform <arch>   Target Docker platform (e.g., linux/arm64, linux/amd64)
  -h, --help          Show this message

EOF
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --simulate|--test)
            DO_SIMULATE=1
            shift
            ;;
        --test-smp)
            TEST_MODULE="test_smp"
            if [ "${DO_SIMULATE}" -eq 0 ]; then
                DO_SIMULATE=1
            fi
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

command -v docker >/dev/null 2>&1 || { echo "Error: docker is required" >&2; exit 1; }

echo "==> Compiling AtomVM for ESP32-P4 under RTEMS 7 inside Docker..."
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

    if [ -f build-host/CMakeCache.txt ] && (grep -q "REBAR3-NOTFOUND" build-host/CMakeCache.txt || grep -q "Darwin" build-host/CMakeCache.txt || grep -q "/usr/bin/cc" build-host/CMakeCache.txt); then
        rm -rf build-host/CMakeCache.txt build-host/CMakeFiles
    fi

    TEST_MOD="'"${TEST_MODULE}"'"
    echo "==> Building host AVM pack (${TEST_MOD})..."
    cmake -S . -B build-host -G Ninja -DAVM_DISABLE_JIT=ON
    cmake --build build-host -t rtems_boot_test rtems_smp_test atomvmlib-rtems
    mkdir -p src/platforms/rtems/build-esp32p4
    ./build-host/tools/packbeam/packbeam create -s "${TEST_MOD}" \
        src/platforms/rtems/build-esp32p4/rtems_boot.avm \
        "build-host/src/platforms/rtems/tests/test_erl_sources/${TEST_MOD}.beam" \
        build-host/libs/atomvmlib-rtems.avm

    echo "==> Cross-compiling AtomVM for riscv/esp32p4db..."
    cmake -S src/platforms/rtems -B src/platforms/rtems/build-esp32p4 -G Ninja \
        -DCMAKE_TOOLCHAIN_FILE=cmake/rtems-toolchain.cmake \
        -DRTEMS_PREFIX=/opt/rtems/7 \
        -DRTEMS_BSP=riscv/esp32p4db \
        -DRTEMS_BOARD=esp32p4 \
        -DRTEMS_VERSION=7 \
        -DAVM_PACK=/workspace/src/platforms/rtems/build-esp32p4/rtems_boot.avm
    cmake --build src/platforms/rtems/build-esp32p4
')
"${RUN_CMD[@]}"

echo "==> Build successful! Direct-boot image is ready at:"
echo "    src/platforms/rtems/build-esp32p4/AtomVM-esp32p4db.bin"

if [ "${DO_SIMULATE}" -eq 1 ]; then
    BATCH_SIZE=50000
    echo "==> Running simulation with esp-emulator..."
    if command -v esp-emu >/dev/null 2>&1; then
        EMU_CMD=(esp-emu --chip esp32p4 --batch-size "${BATCH_SIZE}" --firmware "${REPO_ROOT}/src/platforms/rtems/build-esp32p4/AtomVM-esp32p4db.bin")
        EMU_CMD+=(--timeout 90s --exit-on "Return value: ok")
    else
        EMU_CMD=(docker run)
        if [ -n "${DOCKER_PLATFORM}" ]; then
            EMU_CMD+=(--platform "${DOCKER_PLATFORM}")
        fi
        EMU_CMD+=(--rm -v "${REPO_ROOT}:/workspace" "${IMAGE_NAME}" \
            esp-emu --chip esp32p4 --batch-size "${BATCH_SIZE}" \
            --firmware /workspace/src/platforms/rtems/build-esp32p4/AtomVM-esp32p4db.bin \
            --timeout 90s --exit-on "Return value: ok")
    fi
    if output=$("${EMU_CMD[@]}" 2>&1); then
        printf '%s\n' "$output"
    else
        printf '%s\n' "$output" >&2
        exit 1
    fi
    grep -Fq 'Return value: ok' <<< "$output"
    if [ "${TEST_MODULE}" = test_smp ]; then
        grep -Fq '{smp_parallel_compute,ok}' <<< "$output"
    fi
fi
