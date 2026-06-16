#
# This file is part of AtomVM.
#
# Copyright 2023 Winford (Uncle Grumpy) <winford@object.stream>
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

cmake_minimum_required (VERSION 3.20)
set(CMAKE_C_STANDARD_REQUIRED YES)
set(CMAKE_CXX_STANDARD_REQUIRED YES)

# Enforce to disable any compiler-specific extensions
set(CMAKE_C_EXTENSIONS NO)
set(CMAKE_CXX_EXTENSIONS NO)
set(C_STANDARD_REQUIRED YES)
set_property(GLOBAL PROPERTY C_STANDARD_REQUIRED TRUE)
set_property(GLOBAL PROPERTY CSTD iso9899:2011)
set_property(GLOBAL PROPERTY C_STANDARD iso9899:2011)

find_package(Zephyr REQUIRED HINTS $ENV{ZEPHYR_BASE})
project(AtomVM LANGUAGES C)

if (NOT BOARD)
    message(FATAL_ERROR "No BOARD specified for device config generator")
endif ()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeModules")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

# Options that make sense for this platform
option(AVM_USE_32BIT_FLOAT "Use 32 bit floats." ON)
option(AVM_VERBOSE_ABORT "Print module and line number on VM abort" OFF)
option(AVM_CREATE_STACKTRACES "Create stacktraces" ON)
option(AVM_NEWLIB_NANO "Use 'nano' newlib. Saves 46kB, no `long long` support" OFF)
option(AVM_LOG_DISABLE "Disable log output" OFF)
option(AVM_ENABLE_LOG_COLOR "Use color log output" OFF)
option(AVM_ENABLE_LOG_LINES "Include source and line info for all enabled levels" OFF)
option(AVM_CONFIG_REBOOT_ON_NOT_OK "Reboot when application exits with non 'ok' return" OFF)
option(AVM_DISABLE_GPIO_NIFS "Disable GPIO nifs (input and output)" OFF)
option(AVM_DISABLE_GPIO_PORT_DRIVER "Disable GPIO 'port' driver (input, output, and interrupts)" OFF)
option(AVM_ZEPHYR_EXPERIMENTAL_SMP "Enable experimental AtomVM SMP support on Zephyr" OFF)
option(AVM_ZEPHYR_BOOT_TEST "Boot AtomVM far enough to exercise Zephyr platform initialization" OFF)

if (AVM_ZEPHYR_EXPERIMENTAL_SMP)
    set(AVM_DISABLE_SMP OFF CACHE BOOL "Disable SMP" FORCE)
    set(HAVE_PLATFORM_SMP_H ON CACHE BOOL "Platform has platform_smp.h" FORCE)
else()
    set(AVM_DISABLE_SMP ON CACHE BOOL "Disable SMP" FORCE)
    zephyr_library_compile_definitions(AVM_NO_SMP)
    add_compile_definitions(AVM_NO_SMP)
endif()
set(AVM_DISABLE_TASK_DRIVER OFF CACHE BOOL "Disable Task Driver" FORCE)
add_compile_definitions(AVM_NO_JIT)
add_compile_definitions(_GNU_SOURCE)
add_compile_definitions(_POSIX_C_SOURCE=200809L)
if (NOT AVM_DISABLE_TASK_DRIVER)
    add_compile_definitions(AVM_TASK_DRIVER_ENABLED)
endif()
if (AVM_ZEPHYR_BOOT_TEST)
    add_compile_definitions(AVM_ZEPHYR_BOOT_TEST)
endif()
if (CONFIG_NET_SOCKETS)
    add_compile_definitions(OTP_SOCKET_BSD=1)
endif()

set(HAVE_CLOCK_SETTIME ON FORCE)
set(HAVE_PLATFORM_ATOMIC_H ON CACHE BOOL "Platform has platform_atomic.h" FORCE)
if(CONFIG_SOC_FAMILY_ESPRESSIF_ESP32 AND CONFIG_RISCV)
    # Espressif RISC-V targets such as ESP32-C3 can pass libAtomVM's C11
    # atomic compile check but fail to link __atomic_fetch_* helpers. Use
    # Zephyr's platform atomic shim instead of advertising HAVE_ATOMIC.
    set(ATOMIC_POINTER_LOCK_FREE_IS_TWO FALSE CACHE INTERNAL "Use Zephyr platform atomics" FORCE)
endif()
set(HAVE_RENAME "" CACHE INTERNAL "Have symbol rename" FORCE)

if (AVM_NEWLIB_NANO)
    set(LINKER_FLAGS "${LINKER_FLAGS} -specs=nano.specs")
    set(AVM_LOG_DISABLE ON FORCE)
endif()

if (AVM_CONFIG_REBOOT_ON_NOT_OK)
    add_compile_definitions(CONFIG_REBOOT_ON_NOT_OK)
endif()

# Configure logging
if (AVM_LOG_DISABLE)
    add_compile_definitions(AVM_LOG_DISABLE)
elseif (AVM_LOG_LEVEL_MAX)
    set(AVM_LOG_LEVEL_MAX ${AVM_LOG_LEVEL_MAX} CACHE STRING "AtomVM max log level")
else()
    set(AVM_LOG_LEVEL_MAX LOG_INFO CACHE STRING "AtomVM max log level")
endif()
if (AVM_LOG_LEVEL_MAX)
    set_property(CACHE AVM_LOG_LEVEL_MAX PROPERTY STRINGS LOG_NONE LOG_ERROR LOG_WARN LOG_INFO LOG_DEBUG)
    add_compile_definitions(AVM_LOG_LEVEL_MAX=${AVM_LOG_LEVEL_MAX})
endif()
if (AVM_ENABLE_LOG_COLOR)
    add_compile_definitions(ENABLE_LOG_COLOR)
endif()
if (AVM_ENABLE_LOG_LINES)
    add_compile_definitions(ENABLE_LOG_LINE_INFO)
endif()

# Configure Drivers
if (AVM_DISABLE_GPIO_NIFS)
    add_compile_definitions(AVM_DISABLE_GPIO_NIFS)
endif()
if (AVM_DISABLE_GPIO_PORT_DRIVER)
    add_compile_definitions(AVM_DISABLE_GPIO_PORT_DRIVER)
endif()

## Include additional compilation flags
#include(cmake/compile-flags.cmake)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

# Specify output executable
target_sources(app PRIVATE src/main.c)
target_include_directories(app PRIVATE
    ${CMAKE_CURRENT_BINARY_DIR}/libAtomVM
    ${CMAKE_BINARY_DIR}/zephyr/include/generated)

add_subdirectory(src/lib)

target_link_libraries(app PRIVATE libAtomVM${PLATFORM_LIB_SUFFIX})

set_property(TARGET app PROPERTY C_STANDARD 11)

if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(app PUBLIC -Wall -Wextra -ggdb)
endif()

set(AVM_PORT_ZEPHYR ON CACHE BOOL "Build for Zephyr" FORCE)
set(AVM_DISABLE_JIT ON CACHE BOOL "Disable JIT" FORCE)
add_subdirectory(../../libAtomVM libAtomVM)
target_include_directories(libAtomVM PUBLIC src/lib)
add_dependencies(libAtomVM zephyr_generated_headers)
add_dependencies(libAtomVM${PLATFORM_LIB_SUFFIX} zephyr_generated_headers)
target_link_libraries(app PUBLIC libAtomVM)

message("----------------------------------------")
message(STATUS "Board       : ${BOARD}")
message("--------Device Configuration Info-------")
message(STATUS "Clock Hz    : ${CONFIG_SYS_CLOCK_HW_CYCLES_PER_SEC}")
message(STATUS "Flash Size  : ${CONFIG_FLASH_SIZE}K")
