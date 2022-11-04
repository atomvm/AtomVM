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

if [ -z "${IDF_PATH}" ]; then
    echo "ERROR!  IDF_PATH must be set to root of IDF-SDK to pick up esptool."
    exit -1
fi

filesize="$(ls -s -k ${1} | xargs | cut -d ' ' -f 1)"

echo "%%"
echo "%% Flashing ${1} (size=${filesize}k)"
echo "%%"

exec "${IDF_PATH}/components/esptool_py/esptool/esptool.py" \
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
