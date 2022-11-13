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

cmake_minimum_required(VERSION 3.5)
project (ReleaseEsp32)

## Configure version info for tools
## TODO the following is the contents of AtomVM/CMakeModules/GetVersion.cmake rather than duplicate
## the code here .../GetVersion.cmake and AtomVM/version.cmake should be reworked so they can be used
## here as well as for the "generic_unix" port.

include(${CMAKE_SOURCE_DIR}/../../../version.cmake)

if (ATOMVM_DEV)
    set(ATOMVM_GIT_REVISION "<unknown>")
    execute_process(
        COMMAND git rev-parse --short HEAD
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        OUTPUT_VARIABLE ATOMVM_GIT_REVISION
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    if (NOT ATOMVM_GIT_REVISION STREQUAL "")
        set(ATOMVM_VERSION "${ATOMVM_BASE_VERSION}+git.${ATOMVM_GIT_REVISION}")
    else()
        set(ATOMVM_VERSION ${ATOMVM_BASE_VERSION})
    endif()
else()
    set(ATOMVM_VERSION ${ATOMVM_BASE_VERSION})
endif()

## Build image tools for target chip

set(BOOTLOADER_OFFSET "0x1000")
if(${CONFIG_IDF_TARGET} STREQUAL "esp32s3")
    set(BOOTLOADER_OFFSET "0x0")
elseif(${CONFIG_IDF_TARGET} STREQUAL "esp32c3")
    set(BOOTLOADER_OFFSET "0x0")
elseif( ${CONFIG_IDF_TARGET} STREQUAL "esp32s2")
    set(BOOTLOADER_OFFSET "0x0")
endif()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/mkimage.config.in ${CMAKE_BINARY_DIR}/mkimage.config)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/../../../../tools/dev/flash.sh ${CMAKE_BINARY_DIR}/${CMAKE_FILES_DIRECTORY}/flash.sh COPYONLY)
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

