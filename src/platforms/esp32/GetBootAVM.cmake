#
# This file is part of AtomVM.
#
# Copyright 2025 Winford (Uncle Grumpy) <winford@object.stream>
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

partition_table_get_partition_info(app_offset "--partition-name main.avm" "offset")
if ("${app_offset}" STREQUAL "0x210000")
    set(BOOT_LIBS "esp32boot.avm")
elseif ("${app_offset}" STREQUAL "0x250000")
    set(BOOT_LIBS "elixir_esp32boot.avm")
else()
    message(FATAL_ERROR "Unable to determine esp32boot flavor from offset: ${app_offset}")
endif()

