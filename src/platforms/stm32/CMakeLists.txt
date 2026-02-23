#
# This file is part of AtomVM.
#
# Copyright 2018-2019 Riccardo Binetti <rbino@gmx.com>
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
project(AtomVM)
enable_language(ASM)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeModules")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

# Options that make sense for this platform
option(AVM_ENABLE_OLD_OTP_VERSIONS "Enable OTP version < 26" OFF)
option(AVM_USE_32BIT_FLOAT "Use 32 bit floats." ON)
option(AVM_VERBOSE_ABORT "Print module and line number on VM abort" OFF)
option(AVM_CREATE_STACKTRACES "Create stacktraces" ON)
option(AVM_LOG_DISABLE "Disable log output" OFF)
option(AVM_ENABLE_LOG_COLOR "Use color log output" OFF)
option(AVM_ENABLE_LOG_LINES "Include source and line info for all enabled levels" OFF)
option(AVM_CONFIG_REBOOT_ON_NOT_OK "Reboot when application exits with non 'ok' return" OFF)
option(AVM_DISABLE_GPIO_NIFS "Disable GPIO nifs (input and output)" OFF)
option(AVM_DISABLE_GPIO_PORT_DRIVER "Disable GPIO 'port' driver (input, output, and interrupts)" OFF)
option(AVM_PRINT_PROCESS_CRASH_DUMPS "Print crash reports when processes die with non-standard reasons" ON)

set(AVM_HSE_VALUE "" CACHE STRING "HSE crystal frequency in Hz (e.g. 25000000). Overrides family default.")
if (AVM_HSE_VALUE)
    add_compile_definitions(HSE_VALUE=${AVM_HSE_VALUE}U)
endif()

set(AVM_DISABLE_SMP ON FORCE)
set(AVM_DISABLE_TASK_DRIVER ON FORCE)

if (AVM_CONFIG_REBOOT_ON_NOT_OK)
    add_compile_definitions(CONFIG_REBOOT_ON_NOT_OK)
endif()

    # Configure logging
if (AVM_LOG_DISABLE)
    add_compile_definitions(AVM_LOG_DISABLE)
elseif (AVM_LOG_LEVEL_MAX)
    set(CONFIG_LOG_LEVEL_MAX ${AVM_LOG_LEVEL_MAX} CACHE STRING "AtomVM max log level")
else()
    set(CONFIG_LOG_LEVEL_MAX LOG_INFO CACHE STRING "AtomVM max log level")
endif()
if (CONFIG_LOG_LEVEL_MAX)
    set_property(CACHE CONFIG_LOG_LEVEL_MAX PROPERTY STRINGS LOG_NONE LOG_ERROR LOG_WARN LOG_INFO LOG_DEBUG)
    add_compile_definitions(CONFIG_LOG_LEVEL_MAX=${CONFIG_LOG_LEVEL_MAX})
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

# Include an error in case the user forgets to specify ARM as a toolchain
if (NOT CMAKE_TOOLCHAIN_FILE)
    message(FATAL_ERROR "Cross compiling only. Please use -DCMAKE_TOOLCHAIN_FILE=cmake/arm-toolchain.cmake or use\
    your own toolchain file")
endif ()
mark_as_advanced(CMAKE_TOOLCHAIN_FILE)

option(AVM_DISABLE_JIT "Disable just in time compilation." ON)
if (NOT AVM_DISABLE_JIT)
    set(AVM_JIT_TARGET_ARCH "armv6m")
endif()

if ((NOT ${CMAKE_C_COMPILER_ID} STREQUAL "GNU") OR
    (NOT ${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU") OR
    (${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS 7.2.1))
    message(WARNING "This project is targeted for ARM GCC versions 7.2.1 or above. Some features may not be supported\
     by your compiler. You are using:
     ${CMAKE_CXX_COMPILER_ID} C, version ${CMAKE_C_COMPILER_VERSION}
     ${CMAKE_CXX_COMPILER_ID} CXX, version ${CMAKE_CXX_COMPILER_VERSION}")
endif()

if (NOT DEVICE)
    set(DEVICE stm32f407vgt6)
endif ()

# Bare-metal: nosys.specs provides stubs that make check_function_exists pass,
# but these functions are not actually available. Force-disable all of them.
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

# Include device detection (sets STM32_FAMILY, CPU flags, HAL defines, etc.)
include(cmake/stm32_device.cmake)

# Include auto-device configuration (escript query for clock, ROM, etc.)
include(cmake/atomvm_dev_config.cmake)

# Include SDK fetch (CMSIS + HAL/LL)
include(cmake/stm32_sdk.cmake)

# Include additional compilation flags (must come after dev_config sets CMAKE_FLASH_SIZE)
include(cmake/compile-flags.cmake)

# Build picolibc from source (replaces toolchain newlib to avoid wide char bloat)
include(cmake/picolibc.cmake)

# Use picolibc headers instead of toolchain newlib headers for ALL targets.
# The -isystem path is added globally so libAtomVM also picks up picolibc's
# stdio.h/errno.h instead of newlib's (which pulls in _impure_ptr).
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -isystem ${PICOLIBC_INCLUDE_DIR}")

# Compute linker RAM size (after both stm32_device.cmake and atomvm_dev_config.cmake)
if (LINKER_RAM_SIZE_OVERRIDE)
    set(LINKER_RAM_SIZE "${LINKER_RAM_SIZE_OVERRIDE}K")
else()
    set(LINKER_RAM_SIZE "${RAM_SIZE_KB}K")
endif()

# Generate linker script from template
set(LINKER_SCRIPT_IN "${CMAKE_CURRENT_SOURCE_DIR}/cmake/stm32_linker.ld.in")
set(LINKER_SCRIPT "${CMAKE_CURRENT_BINARY_DIR}/stm32_linker.ld")
configure_file(${LINKER_SCRIPT_IN} ${LINKER_SCRIPT} @ONLY)

# Generate family-specific HAL configuration header from template
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/cmake/stm32_hal_conf.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/generated/${STM32_FAMILY}_hal_conf.h
    @ONLY
)

# Set linker flags
string(REPLACE ";" " " _arch_flags_str "${STM32_ARCH_FLAGS}")
set(LINKER_FLAGS "${LINKER_FLAGS} -nostdlib -nostartfiles -Wl,--gc-sections -T${LINKER_SCRIPT} ${_arch_flags_str}")

message("-----------Linker Info-----------")
message(STATUS "Linker Script : ${LINKER_SCRIPT}")
message(STATUS "Linker Flags  : ${LINKER_FLAGS}")

# Use linker flags to detect symbols
set(CMAKE_TRY_COMPILE_TARGET_TYPE EXECUTABLE)
set(CMAKE_REQUIRED_FLAGS ${LINKER_FLAGS})

add_subdirectory(src)
