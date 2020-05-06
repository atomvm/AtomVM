#!/usr/bin/env bash

set -e

: "${FLASH_BAUD_RATE:=115200}"
: "${FLASH_SERIAL_PORT:=/dev/ttyUSB0}"

if [ -z "${IDF_PATH}" ]; then
    echo "ERROR!  IDF_PATH must be set to root of IDF-SDK to pick up esptool."
    exit -1
fi

filesize="$(ls -s -k ${1} | xargs | cut -d ' ' -f 1)"

echo "%%"
echo "%% Flashing ${1} (size=${filesize}k)"
echo "%%"

exec "${IDF_PATH}/components/esptool_py/esptool/esptool.py" \
    --chip esp32 \
    --port "${FLASH_SERIAL_PORT}" \
    --baud "${FLASH_BAUD_RATE}" \
    --before default_reset \
    --after hard_reset \
    write_flash \
    -u --flash_mode dio --flash_freq 40m \
    --flash_size detect \
    0x110000 \
    "${1}"
