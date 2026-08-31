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

# Run an RTEMS imx7 ELF on QEMU's i.MX 7Dual SABRE machine.
#
# The imx7 BSP copies the FDT from U-Boot (ARM r2). QEMU -kernel of an ELF
# leaves r2=0 and ignores -dtb, so the DTB is raw-loaded and r2 is poked
# via GDB, then GDB detaches.

usage() {
    cat <<EOF
Usage: $(basename "$0") -d dtb [-t timeout_sec] <AtomVM-imx7.exe>

  -d DTB     Device tree blob (required for a console)
  -t SEC     Timeout in seconds (default 90)
EOF
    exit 2
}

DTB=""
TIMEOUT=90
# Keep this in sync with the /memory patch used to create the QEMU DTB.
RAM=1024M
DTB_ADDR=0xb0000000
GDB_PORT=${GDB_PORT:-1234}

while getopts "d:t:h" opt; do
    case "$opt" in
        d) DTB=$OPTARG ;;
        t) TIMEOUT=$OPTARG ;;
        h|*) usage ;;
    esac
done
shift $((OPTIND - 1))

if [ $# -ne 1 ]; then
    usage
fi

ELF=$1
if [ ! -f "$ELF" ]; then
    echo "missing ELF: $ELF" >&2
    exit 1
fi

QEMU=${QEMU:-qemu-system-arm}
if ! command -v "$QEMU" >/dev/null 2>&1; then
    echo "qemu-system-arm not found (set QEMU=...)" >&2
    exit 1
fi

cmd=("$QEMU" -M mcimx7d-sabre -m "$RAM" -nographic -no-reboot -serial mon:stdio -kernel "$ELF")

if [ -z "$DTB" ]; then
    echo "warning: no DTB; the imx7 BSP will not have a console" >&2
    exec timeout "$TIMEOUT" "${cmd[@]}"
fi

if [ ! -f "$DTB" ]; then
    echo "missing DTB: $DTB" >&2
    exit 1
fi

GDB=${GDB:-}
if [ -z "$GDB" ]; then
    for cand in arm-rtems6-gdb gdb-multiarch gdb; do
        if command -v "$cand" >/dev/null 2>&1; then
            GDB=$cand
            break
        fi
    done
fi
if [ -z "$GDB" ]; then
    echo "no gdb found to set r2 (the FDT pointer)" >&2
    exit 1
fi

cmd+=(-device "loader,file=${DTB},addr=${DTB_ADDR},force-raw=on" -S -gdb "tcp::${GDB_PORT}")

timeout "$TIMEOUT" "${cmd[@]}" &
qemu_pid=$!
cleanup() {
    kill "$qemu_pid" 2>/dev/null || true
}
trap cleanup EXIT

gdb_ok=0
for _ in $(seq 1 100); do
    if "$GDB" -q -batch \
        -ex "set pagination off" \
        -ex "target remote 127.0.0.1:${GDB_PORT}" \
        -ex "set \$r2=${DTB_ADDR}" \
        -ex "detach" \
        -ex "quit" >/tmp/atomvm-imx7-gdb.log 2>&1; then
        gdb_ok=1
        break
    fi
    sleep 0.1
done

if [ "$gdb_ok" -ne 1 ]; then
    echo "failed to inject FDT pointer via gdb; log:" >&2
    cat /tmp/atomvm-imx7-gdb.log >&2 || true
    exit 1
fi

set +e
wait "$qemu_pid"
status=$?
set -e
trap - EXIT
exit "$status"
