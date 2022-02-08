#!/usr/bin/env bash
#
#  Copyright 2022 Davide Bettio <davide@uninstall.it>
#
#  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

set -e

ESP32_SOURCES="$1"
. $IDF_PATH/export.sh

cd "${ESP32_SOURCES}"
rm sdkconfig
make defconfig
make
