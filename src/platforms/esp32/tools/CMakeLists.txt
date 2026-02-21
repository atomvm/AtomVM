#
# This file is part of AtomVM.
#
# Copyright 2022 Fred Dushin <fred@dushin.net>
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

cmake_minimum_required (VERSION 3.13)
project (ReleaseEsp32)
include(../../../../version.cmake)
include(../../../../CMakeModules/GetVersion.cmake)
include(../GetBootAVM.cmake)

## Build image tools for target chip

# BOOTLOADER_OFFSET
# https://docs.espressif.com/projects/esp-idf/en/latest/esp32s3/api-guides/bootloader.html

set(BOOTLOADER_OFFSET "0x1000")
if( ${CONFIG_IDF_TARGET} STREQUAL "esp32s2")
    set(BOOTLOADER_OFFSET "0x1000")
elseif(${CONFIG_IDF_TARGET} STREQUAL "esp32s3")
    set(BOOTLOADER_OFFSET "0x0")
elseif(${CONFIG_IDF_TARGET} STREQUAL "esp32c2")
    set(BOOTLOADER_OFFSET "0x0")
elseif(${CONFIG_IDF_TARGET} STREQUAL "esp32c3")
    set(BOOTLOADER_OFFSET "0x0")
elseif(${CONFIG_IDF_TARGET} STREQUAL "esp32c5")
    set(BOOTLOADER_OFFSET "0x2000")
elseif(${CONFIG_IDF_TARGET} STREQUAL "esp32c6")
    set(BOOTLOADER_OFFSET "0x0")
elseif(${CONFIG_IDF_TARGET} STREQUAL "esp32c61")
    set(BOOTLOADER_OFFSET "0x0")
elseif(${CONFIG_IDF_TARGET} STREQUAL "esp32h2")
    set(BOOTLOADER_OFFSET "0x0")
elseif(${CONFIG_IDF_TARGET} STREQUAL "esp32p4")
    set(BOOTLOADER_OFFSET "0x2000")
endif()

if ("${BOOT_LIBS}" STREQUAL "NONE")
    message(WARNING "This appears to be a custom partition map using application offset: ${app_offset}, no AtomVM libraries will be included.")
else()
    if (NOT EXISTS ../../../../build/libs/esp32boot/${BOOT_LIBS})
        message(WARNING "A generic_unix build must be done first in the top level AtomVM/build directory! \n\
        Consult https://doc.atomvm.org/main/build-instructions.html for build instructions.")
    endif()
endif()

set(ROOT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../../..")

if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../nvs_partition.csv")
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/mkimage_nvs.config.in ${CMAKE_BINARY_DIR}/mkimage.config)
else()
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/mkimage.config.in ${CMAKE_BINARY_DIR}/mkimage.config)
endif()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/flash.sh.in ${CMAKE_BINARY_DIR}/${CMAKE_FILES_DIRECTORY}/flash.sh @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/mkimage.erl ${CMAKE_BINARY_DIR}/mkimage.erl COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/flashimage.sh.in ${CMAKE_BINARY_DIR}/${CMAKE_FILES_DIRECTORY}/flashimage.sh @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/mkimage.sh.in ${CMAKE_BINARY_DIR}/${CMAKE_FILES_DIRECTORY}/mkimage.sh @ONLY)
file(COPY ${CMAKE_BINARY_DIR}/${CMAKE_FILES_DIRECTORY}/mkimage.sh
    DESTINATION ${CMAKE_BINARY_DIR}
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)
file(COPY ${CMAKE_BINARY_DIR}/${CMAKE_FILES_DIRECTORY}/flashimage.sh
    DESTINATION ${CMAKE_BINARY_DIR}
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)
file(COPY ${CMAKE_BINARY_DIR}/${CMAKE_FILES_DIRECTORY}/flash.sh
    DESTINATION ${CMAKE_BINARY_DIR}
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)
