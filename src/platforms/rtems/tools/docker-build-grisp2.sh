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

IMAGE_NAME="atomvm-rtems6-grisp2:latest"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"
DOCKERFILE="${REPO_ROOT}/src/platforms/rtems/docker/Dockerfile.rtems6-grisp2"

FORCE_REBUILD=0
CREATE_ZIP=0

usage() {
    cat <<EOF
Usage: $(basename "$0") [options]

Builds AtomVM for GRiSP 2 (ARM i.MX6ULL / LibBSD) inside an isolated Docker container
and prepares the bootable SD-card bundle.

Options:
  --rebuild     Rebuild the Docker container image
  --zip         Compress the SD-card bundle into atomvm-grisp2-sd-card.zip
  -h, --help    Show this message

EOF
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --rebuild)
            FORCE_REBUILD=1
            shift
            ;;
        --zip)
            CREATE_ZIP=1
            shift
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
if [ "${FORCE_REBUILD}" -eq 1 ] || ! docker image inspect "${IMAGE_NAME}" >/dev/null 2>&1; then
    echo "==> Building Docker image '${IMAGE_NAME}' from ${DOCKERFILE}..."
    docker build -t "${IMAGE_NAME}" -f "${DOCKERFILE}" "${REPO_ROOT}/src/platforms/rtems/docker"
fi

# Step 2: Build inside Docker container
echo "==> Compiling AtomVM for GRiSP 2 under RTEMS 6 inside Docker..."
docker run --rm -v "${REPO_ROOT}:/workspace" -w /workspace "${IMAGE_NAME}" bash -c '
    set -euo pipefail

    echo "==> Building host AtomVM and GRiSP 2 test pack..."
    cmake -S . -B build-host -G Ninja -DAVM_DISABLE_JIT=ON
    cmake --build build-host -t rtems_grisp2_test atomvmlib-rtems
    mkdir -p src/platforms/rtems/build-grisp2
    ./build-host/tools/packbeam/packbeam create -s test_grisp2 \
        src/platforms/rtems/build-grisp2/rtems_grisp2.avm \
        build-host/src/platforms/rtems/tests/test_erl_sources/test_grisp2.beam \
        build-host/libs/atomvmlib-rtems.avm

    echo "==> Cross-compiling AtomVM for GRiSP 2 (arm/imx7 with LibBSD)..."
    cmake -S src/platforms/rtems -B src/platforms/rtems/build-grisp2 -G Ninja \
        -DCMAKE_TOOLCHAIN_FILE=cmake/rtems-toolchain.cmake \
        -DRTEMS_PREFIX="${RTEMS_PREFIX}" \
        -DRTEMS_BSP=arm/imx7 \
        -DRTEMS_BOARD=grisp2 \
        -DRTEMS_VERSION=6 \
        -DAVM_PACK=/workspace/src/platforms/rtems/build-grisp2/rtems_grisp2.avm
    cmake --build src/platforms/rtems/build-grisp2

    echo "==> Preparing GRiSP 2 SD-card bundle..."
    bundle="src/platforms/rtems/build-grisp2/sd-card-bundle"
    board="src/platforms/rtems/boards/grisp2"
    mkdir -p "$bundle/debug" "$bundle/dtb-source" "$bundle/loader/entries"
    cp src/platforms/rtems/build-grisp2/AtomVM-grisp2.zImage "$bundle/zImage"
    cp src/platforms/rtems/build-grisp2/oftree "$bundle/"
    cp src/platforms/rtems/build-grisp2/rtems_grisp2.avm "$bundle/app.avm"
    cp src/platforms/rtems/build-grisp2/AtomVM-grisp2.exe "$bundle/debug/AtomVM-grisp2.elf"
    cp "$board/INSTALL.md" "$bundle/README.md"
    cp "$board/atomvm.conf" "$bundle/loader/entries/atomvm.conf"
    cp "$board/imx6ul-grisp2.dtb.license" "$bundle/oftree.license"
    cp "$board/README.md" "$board/COPYING" "$board/grisp2-dtb-source.tar.gz" \
       "$board/grisp2-dtb-source.tar.gz.license" "$bundle/dtb-source/"
    cp "$board/imx6ul-grisp2.dtb" "$board/imx6ul-grisp2.dtb.license" "$bundle/dtb-source/"
    if [ -f "$board/wpa_supplicant.conf.example" ]; then
        cp "$board/wpa_supplicant.conf.example" "$bundle/"
    fi
    cp -R LICENSES "$bundle/"
'

# Step 3: Optional ZIP creation
BUNDLE_DIR="${REPO_ROOT}/src/platforms/rtems/build-grisp2/sd-card-bundle"
ZIP_FILE="${REPO_ROOT}/src/platforms/rtems/build-grisp2/atomvm-grisp2-sd-card.zip"

if [ "${CREATE_ZIP}" -eq 1 ]; then
    echo "==> Creating SD-card bundle ZIP archive..."
    (cd "${BUNDLE_DIR}" && zip -r -q "${ZIP_FILE}" .)
fi

echo ""
echo "================================================================"
echo "GRiSP 2 build successful!"
echo "  Boot Image:   src/platforms/rtems/build-grisp2/AtomVM-grisp2.zImage"
echo "  Device Tree:  src/platforms/rtems/build-grisp2/oftree"
echo "  Smoke Pack:   src/platforms/rtems/build-grisp2/app.avm"
echo "  SD Bundle:    src/platforms/rtems/build-grisp2/sd-card-bundle/"
if [ "${CREATE_ZIP}" -eq 1 ]; then
    echo "  ZIP Archive:  src/platforms/rtems/build-grisp2/atomvm-grisp2-sd-card.zip"
fi
echo ""
echo "To install on your GRiSP 2 SD card:"
echo "  cp src/platforms/rtems/build-grisp2/AtomVM-grisp2.zImage /Volumes/<SD-CARD>/zImage"
echo "  cp src/platforms/rtems/build-grisp2/oftree /Volumes/<SD-CARD>/oftree"
echo "  cp src/platforms/rtems/build-grisp2/app.avm /Volumes/<SD-CARD>/app.avm"
echo "  mkdir -p /Volumes/<SD-CARD>/loader/entries"
echo "  cp src/platforms/rtems/boards/grisp2/atomvm.conf /Volumes/<SD-CARD>/loader/entries/"
echo "================================================================"
