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

IMAGE_NAME="atomvm-rtems7-esp32c3:latest"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"
DOCKERFILE="${REPO_ROOT}/src/platforms/rtems/docker/Dockerfile.rtems7-esp32c3"

DO_SIMULATE=0
FORCE_REBUILD=0
RTEMS_SRC_DIR="${RTEMS_SRC_DIR:-}"
DOCKER_PLATFORM="${DOCKER_PLATFORM:-}"

usage() {
    cat <<EOF
Usage: $(basename "$0") [options]

Builds AtomVM for RTEMS 7 ESP32-C3 inside an isolated Docker container.

Options:
  --simulate          Run the boot test in esp-emulator after compiling
  --rebuild           Rebuild the Docker container image
  --rtems-src <path>  Use local RTEMS source tree (or env RTEMS_SRC_DIR)
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
        --rebuild)
            FORCE_REBUILD=1
            shift
            ;;
        --rtems-src)
            RTEMS_SRC_DIR="$2"
            shift 2
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

# Step 1: Check or build Docker image
BUILD_ARGS=()
PLATFORM_ARGS=()
if [ -n "${DOCKER_PLATFORM}" ]; then
    PLATFORM_ARGS=(--platform "${DOCKER_PLATFORM}")
fi

if [ -n "${RTEMS_SRC_DIR}" ]; then
    if [ ! -d "${RTEMS_SRC_DIR}" ]; then
        echo "Error: Specified RTEMS source directory does not exist: ${RTEMS_SRC_DIR}" >&2
        exit 1
    fi
    RTEMS_SRC_DIR="$(cd "${RTEMS_SRC_DIR}" && pwd)"
    echo "==> Using local RTEMS source tree: ${RTEMS_SRC_DIR}"
    BUILD_ARGS+=(--build-context "rtems-src=${RTEMS_SRC_DIR}")
    FORCE_REBUILD=1
fi

if [ "${FORCE_REBUILD}" -eq 1 ] || ! docker image inspect "${IMAGE_NAME}" >/dev/null 2>&1; then
    echo "==> Building Docker image '${IMAGE_NAME}' from ${DOCKERFILE}..."
    docker build "${PLATFORM_ARGS[@]}" "${BUILD_ARGS[@]}" -t "${IMAGE_NAME}" -f "${DOCKERFILE}" "${REPO_ROOT}/src/platforms/rtems/docker"
fi

# Step 2: Build inside Docker container
echo "==> Compiling AtomVM for ESP32-C3 under RTEMS 7 inside Docker..."
docker run "${PLATFORM_ARGS[@]}" --rm -v "${REPO_ROOT}:/workspace" -w /workspace "${IMAGE_NAME}" bash -c '
    set -euo pipefail

    echo "==> Building host AVM pack..."
    cmake -S . -B build-host -G Ninja -DAVM_DISABLE_JIT=ON
    cmake --build build-host -t rtems_boot_test atomvmlib-rtems
    mkdir -p src/platforms/rtems/build-esp32c3
    ./build-host/tools/packbeam/packbeam create -s test_boot \
        src/platforms/rtems/build-esp32c3/rtems_boot.avm \
        build-host/src/platforms/rtems/tests/test_erl_sources/test_boot.beam \
        build-host/libs/atomvmlib-rtems.avm

    echo "==> Cross-compiling AtomVM for riscv/esp32c3db..."
    cmake -S src/platforms/rtems -B src/platforms/rtems/build-esp32c3 -G Ninja \
        -DCMAKE_TOOLCHAIN_FILE=cmake/rtems-toolchain.cmake \
        -DRTEMS_PREFIX=/opt/rtems/7 \
        -DRTEMS_BSP=riscv/esp32c3db \
        -DRTEMS_BOARD=esp32c3 \
        -DRTEMS_VERSION=7 \
        -DAVM_PACK=/workspace/src/platforms/rtems/build-esp32c3/rtems_boot.avm
    cmake --build src/platforms/rtems/build-esp32c3
'

echo "==> Build successful! Direct-boot image is ready at:"
echo "    src/platforms/rtems/build-esp32c3/AtomVM-esp32c3db.bin"

# Step 3: Run simulation if requested
if [ "${DO_SIMULATE}" -eq 1 ]; then
    echo "==> Running simulation with esp-emulator..."
    if command -v esp-emu >/dev/null 2>&1; then
        esp-emu --chip esp32c3 \
            --firmware "${REPO_ROOT}/src/platforms/rtems/build-esp32c3/AtomVM-esp32c3db.bin" \
            --timeout 30s --exit-on "Return value: ok"
    else
        docker run "${PLATFORM_ARGS[@]}" --rm -v "${REPO_ROOT}:/workspace" "${IMAGE_NAME}" \
            esp-emu --chip esp32c3 \
            --firmware /workspace/src/platforms/rtems/build-esp32c3/AtomVM-esp32c3db.bin \
            --timeout 30s --exit-on "Return value: ok"
    fi
fi
