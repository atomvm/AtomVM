#
# This file is part of AtomVM.
#
# Copyright 2023 Winford <winford@object.stream>
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

# Generate config information for device.
if (NOT DEVICE)
    message(FATAL_ERROR "No DEVICE specified for device config generator")
endif ()

find_program(ESCRIPT escript)
if (NOT ESCRIPT)
    message(FATAL_ERROR "Erlang/OTP 'escript' not found in PATH. Check your Erlang/OTP installation.")
endif ()
mark_as_advanced(ESCRIPT)

set(DEVCONFIG_SCRIPT "${CMAKE_SOURCE_DIR}/tools/atomvm_stm32_config_query.erl")
execute_process(
    COMMAND "${ESCRIPT}" "${DEVCONFIG_SCRIPT}" "${DEVICE}" "clock"
    OUTPUT_VARIABLE DEVCONFIG_CLOCK_HZ
)
add_compile_definitions(${DEVCONFIG_CLOCK_HZ})
execute_process(
    COMMAND "${ESCRIPT}" "${DEVCONFIG_SCRIPT}" "${DEVICE}" "rom"
    OUTPUT_VARIABLE DEVCONFIG_FLASH_SIZE
)
add_compile_definitions(${DEVCONFIG_FLASH_SIZE})
## also needs to be set for correct optomization flags to be used.
set(CMAKE_FLASH_SIZE "${DEVCONFIG_FLASH_SIZE}")

if (AVM_CFG_CONSOLE)
    set(AVM_CFG_CONSOLE ${AVM_CFG_CONSOLE} CACHE STRING "AtomVM system console uart")
    set_property(CACHE AVM_CFG_CONSOLE PROPERTY STRINGS CONSOLE_1 CONSOLE_2 CONSOLE_3 CONSOLE_4 CONSOLE_5 CONSOLE_6 CONSOLE_7 CONSOLE_8)
    add_compile_definitions(${AVM_CFG_CONSOLE})
elseif (BOARD)
    execute_process(
        COMMAND "${ESCRIPT}" "${DEVCONFIG_SCRIPT}" "${DEVICE}" "${BOARD}_com"
        OUTPUT_VARIABLE DEVCONFIG_CONSOLE
    )
    add_compile_definitions(${DEVCONFIG_CONSOLE})
    set(AVM_CFG_CONSOLE ${DEVCONFIG_CONSOLE} CACHE STRING "AtomVM system console uart")
    set(BOARD ${BOARD} CACHE STRING "Board variant configuration")
    set_property(CACHE AVM_CFG_CONSOLE PROPERTY STRINGS CONSOLE_1 CONSOLE_2 CONSOLE_3 CONSOLE_4 CONSOLE_5 CONSOLE_6 CONSOLE_7 CONSOLE_8)
else()
    add_compile_definitions(CONSOLE_2)
    set(AVM_CFG_CONSOLE CONSOLE_2 CACHE STRING "AtomVM system console uart")
    set_property(CACHE AVM_CFG_CONSOLE PROPERTY STRINGS CONSOLE_1 CONSOLE_2 CONSOLE_3 CONSOLE_4 CONSOLE_5 CONSOLE_6 CONSOLE_7 CONSOLE_8)
endif()

message("----------------------------------------")
message(STATUS "Device      : ${DEVICE}")
if (BOARD)
message(STATUS "Board       : ${BOARD}")
endif()
message("--------Device Configuration Info-------")
message(STATUS "Clock Hz    : ${DEVCONFIG_CLOCK_HZ}")
message(STATUS "Flash Size  : ${DEVCONFIG_FLASH_SIZE}")
message(STATUS "Console     : ${AVM_CFG_CONSOLE}")
