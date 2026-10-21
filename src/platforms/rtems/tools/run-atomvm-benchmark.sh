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
# Runs pguyot's atomvm_benchmark suite (https://github.com/pguyot/atomvm_benchmark)
# on an RTEMS ESP32 board under esp-emulator.
#
# The benchmark is built inside the RTEMS Docker image (it needs rebar3/Erlang
# and the cross toolchain), the resulting firmware is written into the mounted
# workspace and then executed with esp-emu on the host.
#
# Note: the suite is heavy and esp-emu is slow; a full pass can take tens of
# minutes. Timings reported by the benchmark are emulated (cycle based), so they
# are deterministic and independent of host speed.

set -euo pipefail

IMAGE_NAME="atomvm-rtems7-esp32:latest"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"

BOARD="esp32c5"
TIMEOUT="1800s"
BATCH_SIZE=""
BENCH_REPO="${BENCH_REPO:-https://github.com/pguyot/atomvm_benchmark.git}"
DOCKER_PLATFORM="${DOCKER_PLATFORM:-}"

usage() {
    cat <<EOF
Usage: $(basename "$0") [options]

Builds and runs the atomvm_benchmark suite on an RTEMS ESP32 board.

Options:
  --board <name>      ESP32 board: esp32c3, esp32c5, esp32c6, esp32s31
                      (default: ${BOARD})
  --timeout <dur>     esp-emu timeout (default: ${TIMEOUT})
  --batch-size <n>    esp-emu --batch-size (default: emulator default)
  -h, --help          Show this message

Environment:
  BENCH_REPO          Override the benchmark git URL
EOF
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --board) BOARD="$2"; shift 2 ;;
        --timeout) TIMEOUT="$2"; shift 2 ;;
        --batch-size) BATCH_SIZE="$2"; shift 2 ;;
        -h|--help) usage ;;
        *) echo "Unknown option: $1" >&2; usage ;;
    esac
done

case "${BOARD}" in
    esp32c3) BSP="riscv/esp32c3db" ;;
    esp32c5) BSP="riscv/esp32c5db" ;;
    esp32c6) BSP="riscv/esp32c6db" ;;
    esp32s31) BSP="riscv/esp32s31db" ;;
    *) echo "Error: unsupported board '${BOARD}'" >&2; exit 1 ;;
esac

BUILD_DIR="src/platforms/rtems/build-${BOARD}"
FIRMWARE="${REPO_ROOT}/${BUILD_DIR}/AtomVM-${BOARD}db.bin"

command -v docker >/dev/null 2>&1 || { echo "Error: docker is required" >&2; exit 1; }

echo "==> Building atomvm_benchmark and AtomVM firmware for ${BOARD} under RTEMS 7..."
RUN_CMD=(docker run)
if [ -n "${DOCKER_PLATFORM}" ]; then
    RUN_CMD+=(--platform "${DOCKER_PLATFORM}")
fi
RUN_CMD+=(--rm -v "${REPO_ROOT}:/workspace" -w /workspace "${IMAGE_NAME}" bash -c '
    set -euo pipefail
    BOARD="'"${BOARD}"'"
    BSP="'"${BSP}"'"
    BENCH_REPO="'"${BENCH_REPO}"'"

    if [ ! -d /root/atomvm_benchmark ]; then
        echo "==> Cloning atomvm_benchmark..."
        git clone --depth 1 "${BENCH_REPO}" /root/atomvm_benchmark
    fi

    echo "==> Building benchmark.avm..."
    ( cd /root/atomvm_benchmark && rebar3 atomvm packbeam >/dev/null )

    echo "==> Building host atomvmlib and packing runnable benchmark..."
    cmake --build build-host -t atomvmlib-rtems
    mkdir -p "src/platforms/rtems/build-${BOARD}"
    ./build-host/tools/packbeam/packbeam create -s benchmark \
        "src/platforms/rtems/build-${BOARD}/benchmark.avm" \
        /root/atomvm_benchmark/_build/default/lib/benchmark/ebin/*.beam \
        build-host/libs/atomvmlib-rtems.avm

    echo "==> Cross-compiling AtomVM for ${BSP} (pack=benchmark)..."
    cmake -S src/platforms/rtems -B "src/platforms/rtems/build-${BOARD}" -G Ninja \
        -DCMAKE_TOOLCHAIN_FILE=cmake/rtems-toolchain.cmake \
        -DRTEMS_PREFIX=/opt/rtems/7 \
        -DRTEMS_BSP="${BSP}" \
        -DRTEMS_BOARD="${BOARD}" \
        -DRTEMS_VERSION=7 \
        -DAVM_PACK="/workspace/src/platforms/rtems/build-${BOARD}/benchmark.avm" >/dev/null
    cmake --build "src/platforms/rtems/build-${BOARD}"
')
"${RUN_CMD[@]}"

echo "==> Firmware ready: ${FIRMWARE}"

echo "==> Running benchmark on ${BOARD} under esp-emulator..."
if command -v esp-emu >/dev/null 2>&1; then
    EMU_CMD=(esp-emu --chip "${BOARD}" --firmware "${FIRMWARE}")
    if [ -n "${BATCH_SIZE}" ]; then
        EMU_CMD+=(--batch-size "${BATCH_SIZE}")
    fi
    EMU_CMD+=(--timeout "${TIMEOUT}" --exit-on "Return value: ok")
    "${EMU_CMD[@]}"
else
    echo "Error: esp-emu not found on PATH" >&2
    exit 1
fi
