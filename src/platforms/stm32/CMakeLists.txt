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

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeModules")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

# Options that make sense for this platform
option(AVM_USE_32BIT_FLOAT "Use 32 bit floats." ON)
option(AVM_VERBOSE_ABORT "Print module and line number on VM abort" OFF)
option(AVM_CREATE_STACKTRACES "Create stacktraces" ON)
option(AVM_NEWLIB_NANO "Use 'nano' newlib. Saves 46kB, no `long long` support" OFF)
option(AVM_LOG_DISABLE "Disable log output" OFF)
option(AVM_ENABLE_LOG_COLOR "Use color log output" OFF)
option(AVM_ENABLE_LOG_LINES "Include source and line info for all enbled levels" OFF)
option(AVM_CONFIG_REBOOT_ON_NOT_OK "Reboot when application exits with non 'ok' return" OFF)
option(AVM_DISABLE_GPIO_NIFS "Disable GPIO nifs (input and output)" OFF)
option(AVM_DISABLE_GPIO_PORT_DRIVER "Disable GPIO 'port' driver (input, output, and interrupts)" OFF)

set(AVM_DISABLE_SMP ON FORCE)

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

# Include auto-device configuration
include(cmake/atomvm_dev_config.cmake)

# Include libopencm3
include(cmake/libopencm3.cmake)

# Include additional compilation flags
include(cmake/compile-flags.cmake)

add_subdirectory(src)
