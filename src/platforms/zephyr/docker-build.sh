#!/bin/bash
#
#  Copyright 2026 Peter M <petermm@gmail.com>
#
#  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
# Helper script to build and run the Zephyr port of AtomVM inside a local Docker container.
#
# Usage:
#   ./docker-build.sh                     - Build the Docker image and compile for nucleo_f429zi
#   ./docker-build.sh -b native_sim_64    - Compile for native_sim_64 (Note: requires host glibc compatibility)
#   ./docker-build.sh -b <board>          - Compile for any specified Zephyr board
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ATOMVM_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
IMAGE_NAME="atomvm-zephyr-build"
BOARD="nucleo_f429zi"

# Parse arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        -b|--board) BOARD="$2"; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

echo "==> Building local Docker image '$IMAGE_NAME'..."
docker build -t "$IMAGE_NAME" -f "$SCRIPT_DIR/Dockerfile" "$SCRIPT_DIR"

echo "==> Compiling AtomVM for board '$BOARD' inside the container..."
docker run --rm \
    -v "$ATOMVM_ROOT:/workspace/zephyr-workspace/AtomVM" \
    -w /workspace/zephyr-workspace/AtomVM/src/platforms/zephyr \
    "$IMAGE_NAME" \
    west build -b "$BOARD" -p=auto .
