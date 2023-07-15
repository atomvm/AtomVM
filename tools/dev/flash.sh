#!/usr/bin/env bash
#
# This file is part of AtomVM.
#
# Copyright 2019-2020 Fred Dushin <fred@dushin.net>
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

set -e

: "${FLASH_BAUD_RATE:=115200}"
: "${FLASH_SERIAL_PORT:=/dev/ttyUSB0}"
: "${FLASH_OFFSET:=0x210000}"

ESP_TOOL=`which esptool.py`
if [ -z ${ESP_TOOL} ]; then
    echo "ERROR!  esptool.py not found! IDF_PATH must be set to root of IDF-SDK to pick up esptool. Or install the standalone tool."
    exit -1
fi

usage() {
    echo "Usage: $ ${0} [-p <port>] [-b <baud>] [-o <offset>] [-l] [-h] <avm_file>"
    echo "Options:"
    echo "    -p <port> is the port connected to the device (default: /dev/ttyUSB0)"
    echo "    -b <baud> is the baud rate (default: 115200)"
    echo "    -o <offset> is the offset into flash to start writing the AVM file"
    echo "    -l (without offset) may be used to flash the AtomVM core libraries (<avm_file>)."
    echo "    -h print this help"
    echo "    <avm-file> is the path to the AVM file"
    echo ""
    echo "Note: '-o' and '-l' should not be used together, both options take precidence over FLASH_OFFSET environment variable."
    echo "      The '-l' option uses the lib.avm partition offset of 0x1D0000"
}

if [[ $# -lt 1 ]]; then
    usage
    exit -1
fi

while getopts 'p:b:o:lh' arg; do
    case ${arg} in
        p)
            FLASH_SERIAL_PORT=${OPTARG}
            ;;
        b)
            FLASH_BAUD_RATE=${OPTARG}
            ;;
        o)
            FLASH_OFFSET=${OPTARG}
            ;;
        l)
            FLASH_OFFSET="0x1D0000"
            ;;
        h)
            usage
            exit 0
            ;;
        *)
            ;;
     esac
done

shift $((OPTIND - 1))

if [[ ! -e "${1}" ]] ; then
    echo "AVM file ${1} does not exist."
    exit 1
fi

filesize="$(ls -s -k ${1} | xargs | cut -d ' ' -f 1)"

echo "%%"
echo "%% Flashing ${1} (size=${filesize}k)"
echo "%%"

exec `which esptool.py` \
    --chip auto \
    --port "${FLASH_SERIAL_PORT}" \
    --baud "${FLASH_BAUD_RATE}" \
    --before default_reset \
    --after hard_reset \
    write_flash \
    -u --flash_mode keep --flash_freq keep \
    --flash_size detect \
    ${FLASH_OFFSET} \
    "${1}"
