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

usage() {
    echo "Usage: $(basename "$0") <esp32c3|esp32c5|esp32c6|esp32h2|esp32s31|esp32p4|esp32p4v1> <build-directory>"
    echo "       $(basename "$0") --check"
    exit "${1:-0}"
}

if [ "${1:-}" = --help ] || [ "${1:-}" = -h ]; then
    usage
fi

command -v wokwi-cli >/dev/null 2>&1 || {
    echo "Error: wokwi-cli is required (https://docs.wokwi.com/wokwi-ci/cli-installation)." >&2
    exit 1
}
if [ -z "${WOKWI_CLI_TOKEN:-}" ]; then
    echo "Error: set WOKWI_CLI_TOKEN in the environment before running Wokwi." >&2
    exit 1
fi
if [ "${1:-}" = --check ] && [ "$#" -eq 1 ]; then
    exit 0
fi
[ "$#" -eq 2 ] || usage 1

CHIP="$1"
TX=TX
RX=RX
CHIP_ATTRS=""
case "${CHIP}" in
    esp32c3) BOARD=board-esp32-c3-devkitm-1 ;;
    esp32c5) BOARD=board-esp32-c5-devkitc-1 ;;
    esp32c6) BOARD=board-esp32-c6-devkitc-1 ;;
    esp32h2) BOARD=board-esp32-h2-devkitm-1 ;;
    esp32s31) BOARD=board-esp32-s31-function-coreboard-1; TX=TX0; RX=RX0 ;;
    esp32p4) BOARD=board-esp32-p4-preview; CHIP_ATTRS=', "chipRevision": "3.1"' ;;
    esp32p4v1) BOARD=board-esp32-p4-preview; CHIP_ATTRS=', "chipRevision": "1.0"' ;;
    *) echo "Error: unsupported Wokwi target: ${CHIP}" >&2; exit 1 ;;
esac

BUILD_DIR="$(cd "$2" && pwd)"
IMAGE="AtomVM-${CHIP}db"
for suffix in bin exe; do
    if [ ! -f "${BUILD_DIR}/${IMAGE}.${suffix}" ]; then
        echo "Error: missing ${BUILD_DIR}/${IMAGE}.${suffix}; build the firmware first." >&2
        exit 1
    fi
done

# Keep generated configuration and serial output with the build artifacts.
# RTEMS direct-boot images must load at offset zero, not the application offset.
mkdir -p "${BUILD_DIR}/wokwi"
cat > "${BUILD_DIR}/wokwi/wokwi.toml" <<EOF
[wokwi]
version = 1
firmware = "../${IMAGE}.bin"
elf = "../${IMAGE}.exe"
EOF
cat > "${BUILD_DIR}/wokwi/diagram.json" <<EOF
{
  "version": 1,
  "author": "AtomVM",
  "editor": "wokwi",
  "parts": [
    { "type": "${BOARD}", "id": "esp", "attrs": { "firmwareOffset": "0", "cpuFrequency": "32"${CHIP_ATTRS} } }
  ],
  "connections": [
    [ "esp:${TX}", "\$serialMonitor:RX", "", [] ],
    [ "esp:${RX}", "\$serialMonitor:TX", "", [] ]
  ]
}
EOF

echo "==> Running ${CHIP} with Wokwi CLI..."
exec wokwi-cli "${BUILD_DIR}/wokwi" --timeout 60000 \
    --expect-text "Return value: ok" --fail-text "*** FATAL ***" \
    --serial-log-file "${BUILD_DIR}/wokwi/serial.log"
