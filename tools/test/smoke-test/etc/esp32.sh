#!/usr/bin/env false
#
# This file is part of AtomVM.
#
# Copyright 2023 Fred Dushin <fred@dushin.net>
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

export ESP32_ESPTOOL="${ESP32_ESPTOOL:-esptool.py}"
export ESP32_CHIP="${ESP32_CHIP:-auto}"
export ESP32_PORT="${ESP32_PORT:-/dev/ttyUSB0}"
export ESP32_BAUD="${ESP32_BAUD:-921600}"
export ESP32_OFFSET="${ESP32_OFFSET:-0x1000}"
