#!/bin/bash
#
#  Copyright 2026 Peter M <petermm@gmail.com>
#
#  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
# Helper script to run Zephyr simulator tests for AtomVM inside a local Docker container.
#
# Usage:
#   ./docker-test.sh                         - Run tests for qemu_x86_64
#   ./docker-test.sh -b qemu_x86_64          - Run tests for qemu_x86_64
#   ./docker-test.sh -b <board>              - Run tests for any specified Zephyr board
#   ./docker-test.sh -b <board> -- <args>    - Pass extra arguments to west twister
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ATOMVM_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
IMAGE_NAME="atomvm-zephyr-build"
BOARD="qemu_x86_64"
TEST_ROOT="AtomVM/src/platforms/zephyr/tests"
OUT_DIR="AtomVM/src/platforms/zephyr/twister-out"
EXTRA_TWISTER_ARGS=()

# Parse arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        -b|--board) BOARD="$2"; shift ;;
        --) shift; EXTRA_TWISTER_ARGS=("$@"); break ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

echo "==> Building local Docker image '$IMAGE_NAME'..."
docker build -t "$IMAGE_NAME" -f "$SCRIPT_DIR/Dockerfile" "$SCRIPT_DIR"

if [[ "$BOARD" == "native_sim" ]]; then
    BOARD="native_sim/native/64"
fi

echo "==> Running Zephyr tests for board '$BOARD' inside the container..."
if [[ "$BOARD" == esp32* ]]; then
    echo "==> Note: qemu_x86_64 runs in simulation; ESP32 scenarios are build-only until a simulator/device harness is added."
fi
docker run --rm \
    -e ASAN_OPTIONS=detect_leaks=0 \
    -e LSAN_OPTIONS=exitcode=0 \
    -v "$ATOMVM_ROOT:/workspace/zephyr-workspace/AtomVM" \
    -w /workspace/zephyr-workspace \
    "$IMAGE_NAME" \
    west twister -T "$TEST_ROOT" -p "$BOARD" --inline-logs -W --outdir "$OUT_DIR" "${EXTRA_TWISTER_ARGS[@]}"

