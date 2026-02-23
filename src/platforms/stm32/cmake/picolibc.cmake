#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

# Build picolibc from source using meson, configured without wide char/multibyte
# support but with long-long and C99 format support.

include(ExternalProject)

find_program(MESON_EXECUTABLE meson REQUIRED)
find_program(NINJA_EXECUTABLE ninja REQUIRED)

# Allow using a local picolibc source checkout via environment variable or CMake variable.
if (DEFINED ENV{PICOLIBC_PATH} AND (NOT PICOLIBC_PATH))
    set(PICOLIBC_PATH $ENV{PICOLIBC_PATH})
    message("Using PICOLIBC_PATH from environment ('${PICOLIBC_PATH}')")
endif ()

set(PICOLIBC_VERSION "1.8.11")
set(PICOLIBC_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/_deps/picolibc")
if (PICOLIBC_PATH)
    get_filename_component(PICOLIBC_SOURCE_DIR "${PICOLIBC_PATH}" REALPATH)
else()
    set(PICOLIBC_SOURCE_DIR "${PICOLIBC_PREFIX}/src/picolibc")
endif()
set(PICOLIBC_BUILD_DIR "${PICOLIBC_PREFIX}/build")
set(PICOLIBC_INSTALL_DIR "${PICOLIBC_PREFIX}/install")

# Build the arch flags string for the meson cross-file c_args
# STM32_ARCH_FLAGS is a CMake list; we need to format it as a meson array
set(_meson_c_args "")
foreach(_flag ${STM32_ARCH_FLAGS})
    if (_meson_c_args)
        string(APPEND _meson_c_args ", ")
    endif()
    string(APPEND _meson_c_args "'${_flag}'")
endforeach()

# Generate meson cross-file
set(PICOLIBC_CROSS_FILE "${PICOLIBC_PREFIX}/cross-file.txt")
file(WRITE "${PICOLIBC_CROSS_FILE}" "\
[binaries]
c = '${ARM_CC}'
ar = '${ARM_AR}'
as = '${ARM_AS}'
nm = '${ARM_NM}'
strip = '${ARM_STRIP}'

[host_machine]
system = 'none'
cpu_family = 'arm'
cpu = '${STM32_CPU}'
endian = 'little'

[built-in options]
c_args = [${_meson_c_args}]
")

if (PICOLIBC_PATH)
    set(_picolibc_download_args DOWNLOAD_COMMAND "")
    message(STATUS "Using local picolibc source: ${PICOLIBC_SOURCE_DIR}")
else()
    set(_picolibc_download_args
        URL "https://github.com/picolibc/picolibc/releases/download/${PICOLIBC_VERSION}/picolibc-${PICOLIBC_VERSION}.tar.xz"
        URL_HASH SHA256=b4671ddeecbe427b77fa9a863be92c74a9059e0373a5b31343c791ff2337db64
        DOWNLOAD_EXTRACT_TIMESTAMP TRUE
        LOG_DOWNLOAD TRUE
    )
    if (NOT EXISTS "${PICOLIBC_SOURCE_DIR}/meson.build")
        message("Downloading and building picolibc ${PICOLIBC_VERSION}")
    endif()
endif()

ExternalProject_Add(picolibc
    ${_picolibc_download_args}
    PREFIX "${PICOLIBC_PREFIX}"
    SOURCE_DIR "${PICOLIBC_SOURCE_DIR}"
    BINARY_DIR "${PICOLIBC_BUILD_DIR}"
    INSTALL_DIR "${PICOLIBC_INSTALL_DIR}"
    CONFIGURE_COMMAND ${MESON_EXECUTABLE} setup
        --cross-file ${PICOLIBC_CROSS_FILE}
        --prefix <INSTALL_DIR>
        -Dmultilib=false
        -Dformat-default=l
        -Dio-c99-formats=true
        -Dio-long-long=true
        -Dmb-capable=false
        -Dpicocrt=false
        -Dsemihost=false
        -Dtests=false
        -Dposix-console=true
        -Dsingle-thread=true
        -Dthread-local-storage=false
        -Dnewlib-global-errno=true
        -Dspecsdir=none
        <BINARY_DIR>
        <SOURCE_DIR>
    BUILD_COMMAND ${NINJA_EXECUTABLE} -C <BINARY_DIR>
    INSTALL_COMMAND ${NINJA_EXECUTABLE} -C <BINARY_DIR> install
    BUILD_BYPRODUCTS
        "${PICOLIBC_INSTALL_DIR}/lib/libc.a"
)

# Export variables for other CMake files to use
set(PICOLIBC_INCLUDE_DIR "${PICOLIBC_INSTALL_DIR}/include" CACHE INTERNAL "")
set(PICOLIBC_LIB_DIR "${PICOLIBC_INSTALL_DIR}/lib" CACHE INTERNAL "")
