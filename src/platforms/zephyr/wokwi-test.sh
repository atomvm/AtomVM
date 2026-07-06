#!/bin/bash
#
#  Copyright 2026 Peter M <petermm@gmail.com>
#
#  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
# Helper script to run local Wokwi simulation tests for the Zephyr port of AtomVM.
#
# Usage:
#   ./wokwi-test.sh [-b board] [--skip-build]
#
# Default board is esp32_devkitc/esp32/procpu.
# Specify --skip-build to run the simulation on the last built binaries without rebuild.
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ATOMVM_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
IMAGE_NAME="atomvm-zephyr-build"
BOARD="esp32_devkitc/esp32/procpu"
SKIP_BUILD=false

# Parse arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        -b|--board) BOARD="$2"; shift ;;
        --skip-build) SKIP_BUILD=true ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

# We only support board options that map to Wokwi parts.
# Currently we check for esp32-devkit-c-v4 and pi-pico.
if [[ "$BOARD" == *"esp32"* ]]; then
    WOKWI_BOARD_TYPE="board-esp32-devkit-c-v4"
    WOKWI_OFFSET="0x1000"
    FIRMWARE_FILE="zephyr.bin"
elif [[ "$BOARD" == *"pico"* || "$BOARD" == *"rp2"* ]]; then
    WOKWI_BOARD_TYPE="wokwi-pi-pico"
    WOKWI_OFFSET=""
    FIRMWARE_FILE="zephyr.elf"
else
    echo "Warning: Unsupported board for Wokwi CLI simulation. Defaulting to esp32_devkitc."
    BOARD="esp32_devkitc/esp32/procpu"
    WOKWI_BOARD_TYPE="board-esp32-devkit-c-v4"
    WOKWI_OFFSET="0x1000"
    FIRMWARE_FILE="zephyr.bin"
fi

BUILD_DIR="build-wokwi-test"
BOARD_CLEAN="${BOARD//\//_}"
SIM_DIR="$SCRIPT_DIR/twister-out/wokwi-${BOARD_CLEAN}"

if [ "$SKIP_BUILD" = false ]; then
    echo "==> Building local Docker image '$IMAGE_NAME'..."
    docker build -t "$IMAGE_NAME" -f "$SCRIPT_DIR/Dockerfile" "$SCRIPT_DIR"

    echo "==> Compiling Zephyr tests for board '$BOARD' inside the container..."
    docker run --rm \
        -v "$ATOMVM_ROOT:/workspace/zephyr-workspace/AtomVM" \
        -w /workspace/zephyr-workspace \
        "$IMAGE_NAME" \
        west build -b "$BOARD" -d "AtomVM/src/platforms/zephyr/${BUILD_DIR}" -p=auto AtomVM/src/platforms/zephyr/tests
fi

# Locate build binaries on host
HOST_BUILD_DIR="$SCRIPT_DIR/${BUILD_DIR}/zephyr"
if [ ! -f "$HOST_BUILD_DIR/zephyr.bin" ] || [ ! -f "$HOST_BUILD_DIR/zephyr.elf" ]; then
    echo "Error: Binaries not found at $HOST_BUILD_DIR. Please build them first."
    exit 1
fi

echo "==> Staging Wokwi simulation files in $SIM_DIR..."
mkdir -p "$SIM_DIR"
cp "$HOST_BUILD_DIR/zephyr.bin" "$SIM_DIR/"
cp "$HOST_BUILD_DIR/zephyr.elf" "$SIM_DIR/"

# Create wokwi.toml
cat <<EOF > "$SIM_DIR/wokwi.toml"
[wokwi]
version = 1
firmware = "${FIRMWARE_FILE}"
elf = "zephyr.elf"
EOF

# Create diagram.json
if [ -n "$WOKWI_OFFSET" ]; then
    # ESP32
    cat <<EOF > "$SIM_DIR/diagram.json"
{
  "version": 1,
  "author": "AtomVM",
  "editor": "wokwi",
  "parts": [
    {
      "type": "${WOKWI_BOARD_TYPE}",
      "id": "esp",
      "top": 0,
      "left": 0,
      "attrs": {
        "firmwareOffset": "${WOKWI_OFFSET}"
      }
    }
  ],
  "connections": [
    ["esp:TX", "\$serialMonitor:RX", "", []],
    ["esp:RX", "\$serialMonitor:TX", "", []]
  ]
}
EOF
else
    # Pico
    cat <<EOF > "$SIM_DIR/diagram.json"
{
  "version": 1,
  "author": "AtomVM",
  "editor": "wokwi",
  "parts": [
    {
      "type": "${WOKWI_BOARD_TYPE}",
      "id": "pico",
      "top": 0,
      "left": 0
    }
  ],
  "connections": [
    ["pico:GP0", "\$serialMonitor:RX", "", []],
    ["pico:GP1", "\$serialMonitor:TX", "", []]
  ]
}
EOF
fi

# Run wokwi-cli
echo "==> Running Wokwi simulation..."
if [ -z "$WOKWI_CLI_TOKEN" ]; then
    echo "Warning: WOKWI_CLI_TOKEN is not set. The simulation might fail if authentication is required."
fi

cd "$SIM_DIR"
wokwi-cli --timeout 20000 --expect-text "PROJECT EXECUTION SUCCESSFUL" .
