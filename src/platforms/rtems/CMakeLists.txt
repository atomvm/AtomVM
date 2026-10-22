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
option(AVM_MINIMAL_OPCODES "Exclude opcodes for optional compiler flags; requires compatible BEAM files" OFF)
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
elseif(RTEMS_ARCH STREQUAL "riscv" AND RTEMS_BSP_NAME MATCHES "esp32")
    if(RTEMS_BSP_NAME MATCHES "esp32c6")
        set(_rtems_board_default "esp32c6")
    elseif(RTEMS_BSP_NAME MATCHES "esp32c5")
        set(_rtems_board_default "esp32c5")
    elseif(RTEMS_BSP_NAME MATCHES "esp32s31")
        set(_rtems_board_default "esp32s31")
    else()
        set(_rtems_board_default "esp32c3")
    endif()
endif()
set(RTEMS_BOARD "${_rtems_board_default}" CACHE STRING
    "RTEMS board integration (generic, qemu, grisp2, esp32c3, esp32c5, esp32c6, or esp32s31)")
set_property(CACHE RTEMS_BOARD PROPERTY STRINGS generic qemu grisp2 esp32c3 esp32c5 esp32c6 esp32s31)

if(RTEMS_BOARD MATCHES "^esp32")
    if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
        set(CMAKE_BUILD_TYPE MinSizeRel CACHE STRING "Build type" FORCE)
    endif()
    # Apply on reconfiguration too; toolchain *_FLAGS_INIT only affects new caches.
    # ESP BSP scripts KEEP startup code, constructors and RTEMS linker sets.
    add_compile_options(-ffunction-sections -fdata-sections)
    add_link_options(-Wl,--gc-sections)
endif()

if(NOT RTEMS_BOARD STREQUAL "generic" AND
   NOT RTEMS_BOARD STREQUAL "qemu" AND
   NOT RTEMS_BOARD STREQUAL "grisp2" AND
   NOT RTEMS_BOARD STREQUAL "esp32c3" AND
   NOT RTEMS_BOARD STREQUAL "esp32c5" AND
   NOT RTEMS_BOARD STREQUAL "esp32c6" AND
   NOT RTEMS_BOARD STREQUAL "esp32s31")
    message(FATAL_ERROR
        "Unsupported RTEMS_BOARD='${RTEMS_BOARD}' (use generic, qemu, grisp2, esp32c3, esp32c5, esp32c6, or esp32s31)")
endif()

if(RTEMS_BOARD STREQUAL "grisp2" AND
   (NOT RTEMS_ARCH STREQUAL "arm" OR NOT RTEMS_BSP_NAME STREQUAL "imx7"))
    message(FATAL_ERROR "RTEMS_BOARD=grisp2 requires RTEMS_BSP=arm/imx7")
endif()
if(RTEMS_BOARD STREQUAL "grisp2" AND NOT RTEMS_VERSION STREQUAL "6")
    message(FATAL_ERROR "RTEMS_BOARD=grisp2 requires RTEMS_VERSION=6")
endif()
if((RTEMS_BOARD STREQUAL "esp32c3" OR RTEMS_BOARD STREQUAL "esp32c5" OR RTEMS_BOARD STREQUAL "esp32c6" OR RTEMS_BOARD STREQUAL "esp32s31") AND
   (NOT RTEMS_ARCH STREQUAL "riscv" OR NOT RTEMS_BSP_NAME MATCHES "esp32"))
    message(FATAL_ERROR "RTEMS_BOARD=${RTEMS_BOARD} requires a RISC-V esp32 BSP (e.g. riscv/esp32c3db, riscv/esp32c5db, riscv/esp32c6db, or riscv/esp32s31db)")
endif()

option(RTEMS_BT "Build ESP32 Bluetooth controller support (C3 or C5)" OFF)
option(AVM_ENABLE_BLE_VHCI_NIF "Build the ESP32-C5 Erlang BLE VHCI interface" OFF)
if(AVM_ENABLE_BLE_VHCI_NIF AND (NOT RTEMS_BT OR NOT RTEMS_BOARD STREQUAL "esp32c5"))
    message(FATAL_ERROR "AVM_ENABLE_BLE_VHCI_NIF requires RTEMS_BT=ON and RTEMS_BOARD=esp32c5")
endif()
set(RTEMS_WIFI_POWER_SAVE "NONE" CACHE STRING "Experimental ESP Wi-Fi power saving: NONE, MIN_MODEM or MAX_MODEM")
set_property(CACHE RTEMS_WIFI_POWER_SAVE PROPERTY TYPE STRING)
set_property(CACHE RTEMS_WIFI_POWER_SAVE PROPERTY STRINGS NONE MIN_MODEM MAX_MODEM)
string(TOUPPER "${RTEMS_WIFI_POWER_SAVE}" RTEMS_WIFI_POWER_SAVE_MODE)
if(RTEMS_WIFI_POWER_SAVE_MODE STREQUAL "ON")
    set(RTEMS_WIFI_POWER_SAVE_MODE MIN_MODEM)
elseif(RTEMS_WIFI_POWER_SAVE_MODE STREQUAL "OFF")
    set(RTEMS_WIFI_POWER_SAVE_MODE NONE)
endif()
if(NOT RTEMS_WIFI_POWER_SAVE_MODE MATCHES "^(NONE|MIN_MODEM|MAX_MODEM)$")
    message(FATAL_ERROR "RTEMS_WIFI_POWER_SAVE must be NONE, MIN_MODEM or MAX_MODEM")
endif()
set(RTEMS_WIFI_LISTEN_INTERVAL "3" CACHE STRING "Wi-Fi MAX_MODEM listen interval in AP beacon intervals")
if(NOT RTEMS_WIFI_LISTEN_INTERVAL MATCHES "^[1-9][0-9]*$" OR RTEMS_WIFI_LISTEN_INTERVAL GREATER 65535)
    message(FATAL_ERROR "RTEMS_WIFI_LISTEN_INTERVAL must be between 1 and 65535")
endif()
if(RTEMS_BT AND NOT (RTEMS_BOARD STREQUAL "esp32c3" OR RTEMS_BOARD STREQUAL "esp32c5"))
    message(FATAL_ERROR "RTEMS_BT requires RTEMS_BOARD=esp32c3 or esp32c5")
endif()

include(CheckCSourceCompiles)
set(_saved_req_flags "${CMAKE_REQUIRED_FLAGS}")
set(CMAKE_REQUIRED_FLAGS "${CMAKE_C_FLAGS}")
check_c_source_compiles("
#include <rtems/score/cpuopts.h>
#ifndef RTEMS_SMP
#error \"RTEMS_SMP not defined\"
#endif
int main(void) { return 0; }
" RTEMS_BSP_HAS_SMP)
set(CMAKE_REQUIRED_FLAGS "${_saved_req_flags}")

if(RTEMS_BSP_HAS_SMP AND NOT RTEMS_BOARD STREQUAL "grisp2")
    set(DEFAULT_AVM_DISABLE_SMP OFF)
else()
    set(DEFAULT_AVM_DISABLE_SMP ON)
endif()

option(AVM_DISABLE_SMP "Disable SMP support." ${DEFAULT_AVM_DISABLE_SMP})
if(RTEMS_ARCH STREQUAL "riscv" AND RTEMS_BSP_NAME MATCHES "esp32")
    set(AVM_DISABLE_TASK_DRIVER OFF CACHE BOOL "Disable task driver support." FORCE)
    set(HAVE_PLATFORM_ATOMIC_H ON)
else()
    set(AVM_DISABLE_TASK_DRIVER ON CACHE BOOL "Disable task driver support." FORCE)
endif()

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

set(RTEMS_HAS_LWIP OFF)
set(RTEMS_HAS_ESP_WIFI OFF)
if(RTEMS_ARCH STREQUAL "riscv" AND RTEMS_BSP_NAME MATCHES "esp32")
    set(_rtems_lwip "${RTEMS_PREFIX}/${RTEMS_TARGET}/${RTEMS_BSP_NAME}/lib/liblwip.a")
    if(EXISTS "${_rtems_lwip}")
        set(RTEMS_HAS_LWIP ON)
        set(HAVE_SOCKET 1 CACHE INTERNAL "Have symbol socket" FORCE)
        set(HAVE_SELECT 1 CACHE INTERNAL "Have symbol select" FORCE)
        message(STATUS "Linking rtems-lwip from ${_rtems_lwip}")
    else()
        message(STATUS
            "rtems-lwip not found at ${_rtems_lwip}; building riscv/esp32 without socket support")
    endif()

    set(_rtems_esp_wifi "${RTEMS_PREFIX}/${RTEMS_TARGET}/${RTEMS_BSP_NAME}/lib/librtems_esp_wifi.a")
    if(EXISTS "${_rtems_esp_wifi}")
        set(RTEMS_HAS_ESP_WIFI ON)
        message(STATUS "Enabling ESP32 Wi-Fi stack from ${_rtems_esp_wifi}")
    endif()
endif()

if(RTEMS_BT AND NOT RTEMS_HAS_ESP_WIFI)
    message(FATAL_ERROR "RTEMS_BT requires the ESP32 Wi-Fi stack at ${_rtems_esp_wifi}")
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
