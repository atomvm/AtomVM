#
# This file is part of AtomVM.
#
# Copyright 2026 Peter M. <petermm@gmail.com>
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

cmake_minimum_required(VERSION 3.13)
project(AtomVM)
enable_language(ASM)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeModules")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

if(NOT CMAKE_TOOLCHAIN_FILE)
    message(FATAL_ERROR "Cross compiling only. Please use -DCMAKE_TOOLCHAIN_FILE=cmake/rtems-toolchain.cmake")
endif()
mark_as_advanced(CMAKE_TOOLCHAIN_FILE)

option(AVM_USE_32BIT_FLOAT "Use 32 bit floats." OFF)
option(AVM_VERBOSE_ABORT "Print module and line number on VM abort" OFF)
option(AVM_CREATE_STACKTRACES "Create stacktraces" ON)
option(AVM_DISABLE_JIT "Disable just in time compilation." ON)
option(AVM_PRINT_PROCESS_CRASH_DUMPS "Print crash reports when processes die with non-standard reasons" ON)
option(AVM_CONFIG_REBOOT_ON_NOT_OK "Reboot when application exits with non 'ok' return" OFF)

set(_rtems_board_default "generic")
if(RTEMS_ARCH STREQUAL "arm" AND RTEMS_BSP_NAME STREQUAL "imx7")
    # The BSP is shared by the QEMU SABRE model and GRiSP 2.  Keep the
    # existing generic/QEMU behavior as the default and require an explicit
    # board selection for physical-board assumptions.
    set(_rtems_board_default "qemu")
endif()
set(RTEMS_BOARD "${_rtems_board_default}" CACHE STRING
    "RTEMS board integration (generic, qemu, or grisp2)")
set_property(CACHE RTEMS_BOARD PROPERTY STRINGS generic qemu grisp2)

if(NOT RTEMS_BOARD STREQUAL "generic" AND
   NOT RTEMS_BOARD STREQUAL "qemu" AND
   NOT RTEMS_BOARD STREQUAL "grisp2")
    message(FATAL_ERROR
        "Unsupported RTEMS_BOARD='${RTEMS_BOARD}' (use generic, qemu, or grisp2)")
endif()

if(RTEMS_BOARD STREQUAL "grisp2" AND
   (NOT RTEMS_ARCH STREQUAL "arm" OR NOT RTEMS_BSP_NAME STREQUAL "imx7"))
    message(FATAL_ERROR "RTEMS_BOARD=grisp2 requires RTEMS_BSP=arm/imx7")
endif()
if(RTEMS_BOARD STREQUAL "grisp2" AND NOT RTEMS_VERSION STREQUAL "6")
    message(FATAL_ERROR "RTEMS_BOARD=grisp2 requires RTEMS_VERSION=6")
endif()

set(AVM_DISABLE_SMP ON CACHE BOOL "Disable SMP support." FORCE)
set(AVM_DISABLE_TASK_DRIVER ON CACHE BOOL "Disable task driver support." FORCE)

if(NOT AVM_DISABLE_JIT)
    message(WARNING "JIT is not supported on the RTEMS platform yet; disabling.")
    set(AVM_DISABLE_JIT ON CACHE BOOL "Disable just in time compilation." FORCE)
endif()

if(AVM_CONFIG_REBOOT_ON_NOT_OK)
    add_compile_definitions(CONFIG_REBOOT_ON_NOT_OK)
endif()

# RTEMS newlib may declare POSIX APIs that are not provided without libbsd / IMFS.
# Force the same conservative defaults used by other embedded ports. Socket and
# select are re-enabled below when LibBSD is linked for imx7.
set(HAVE_MKFIFO "" CACHE INTERNAL "Have symbol mkfifo" FORCE)
set(HAVE_UNLINK "" CACHE INTERNAL "Have symbol unlink" FORCE)
set(HAVE_EXECVE "" CACHE INTERNAL "Have symbol execve" FORCE)
set(HAVE_OPEN "" CACHE INTERNAL "Have symbol open" FORCE)
set(HAVE_CLOSE "" CACHE INTERNAL "Have symbol close" FORCE)
set(HAVE_OPENDIR "" CACHE INTERNAL "Have symbol opendir" FORCE)
set(HAVE_CLOSEDIR "" CACHE INTERNAL "Have symbol closedir" FORCE)
set(HAVE_READDIR "" CACHE INTERNAL "Have symbol readdir" FORCE)
set(HAVE_CLOSEFROM "" CACHE INTERNAL "Have symbol closefrom" FORCE)
set(HAVE_GETCWD "" CACHE INTERNAL "Have symbol getcwd" FORCE)
set(HAVE_CLOCK_SETTIME "" CACHE INTERNAL "Have symbol clock_settime" FORCE)
set(HAVE_SETTIMEOFDAY "" CACHE INTERNAL "Have symbol settimeofday" FORCE)
set(HAVE_SOCKET "" CACHE INTERNAL "Have symbol socket" FORCE)
set(HAVE_SELECT "" CACHE INTERNAL "Have symbol select" FORCE)
set(HAVE_LSEEK "" CACHE INTERNAL "Have symbol lseek" FORCE)
set(HAVE_PREAD "" CACHE INTERNAL "Have symbol pread" FORCE)
set(HAVE_PWRITE "" CACHE INTERNAL "Have symbol pwrite" FORCE)
set(HAVE_FSYNC "" CACHE INTERNAL "Have symbol fsync" FORCE)
set(HAVE_FTRUNCATE "" CACHE INTERNAL "Have symbol ftruncate" FORCE)
set(HAVE_RENAME "" CACHE INTERNAL "Have symbol rename" FORCE)
set(HAVE_STAT "" CACHE INTERNAL "Have symbol stat" FORCE)
set(HAVE_FSTAT "" CACHE INTERNAL "Have symbol fstat" FORCE)
set(HAVE_MKDIR "" CACHE INTERNAL "Have symbol mkdir" FORCE)
set(HAVE_RMDIR "" CACHE INTERNAL "Have symbol rmdir" FORCE)
set(HAVE_POSIX_SPAWN_CLOEXEC_DEFAULT "" CACHE INTERNAL "Have symbol POSIX_SPAWN_CLOEXEC_DEFAULT" FORCE)

set(RTEMS_HAS_LIBBSD OFF)
if(RTEMS_ARCH STREQUAL "arm" AND RTEMS_BSP_NAME STREQUAL "imx7")
    set(_rtems_libbsd "${RTEMS_PREFIX}/${RTEMS_TARGET}/${RTEMS_BSP_NAME}/lib/libbsd.a")
    if(EXISTS "${_rtems_libbsd}")
        set(RTEMS_HAS_LIBBSD ON)
        set(HAVE_SOCKET 1 CACHE INTERNAL "Have symbol socket" FORCE)
        set(HAVE_SELECT 1 CACHE INTERNAL "Have symbol select" FORCE)
        message(STATUS "Linking rtems-libbsd from ${_rtems_libbsd}")
    else()
        message(STATUS
            "rtems-libbsd not found at ${_rtems_libbsd}; building arm/imx7 without socket support")
    endif()
endif()

if(RTEMS_BOARD STREQUAL "grisp2")
    if(NOT RTEMS_HAS_LIBBSD)
        message(FATAL_ERROR
            "RTEMS_BOARD=grisp2 requires RTEMS 6 LibBSD for arm/imx7 at ${_rtems_libbsd}")
    endif()
    set(RTEMS_BOARD_FDT_COMPATIBLE "embeddedbrains,grisp2")
    set(RTEMS_DTB "${CMAKE_CURRENT_SOURCE_DIR}/boards/grisp2/imx6ul-grisp2.dtb"
        CACHE FILEPATH "Board device tree passed separately by the bootloader")
    set(RTEMS_IMAGE_LOAD_ADDRESS "0x80200000" CACHE STRING
        "Boot image load and entry address")
    set(_rtems_create_zimage_default ON)
else()
    set(_rtems_create_zimage_default OFF)
endif()
option(RTEMS_CREATE_ZIMAGE
    "Create a bootloader-ready zImage next to the RTEMS ELF"
    ${_rtems_create_zimage_default})
if(RTEMS_CREATE_ZIMAGE AND NOT RTEMS_BOARD STREQUAL "grisp2")
    message(FATAL_ERROR "RTEMS_CREATE_ZIMAGE currently requires RTEMS_BOARD=grisp2")
endif()

set(AVM_PACK "" CACHE FILEPATH "AVM pack to embed in the RTEMS executable")

add_subdirectory(src)
