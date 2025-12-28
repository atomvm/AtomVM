#
# This file is part of AtomVM.
#
# Copyright 2022 Davide Bettio <davide@uninstall.it>
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
include($ENV{IDF_PATH}/tools/cmake/project.cmake)

# Don't try to link when testing availability of functions
set(CMAKE_TRY_COMPILE_TARGET_TYPE "STATIC_LIBRARY")

# mkfifo is defined in newlib header but not implemented
set(HAVE_MKFIFO "" CACHE INTERNAL "Have symbol mkfifo" FORCE)
# Likewise with EXECVE
set(HAVE_EXECVE "" CACHE INTERNAL "Have symbol execve" FORCE)
# Force HAVE_SOCKET
# Automatically detecting it requires to put too many components include dirs
# in CMAKE_REQUIRED_INCLUDES as lwip includes freetos and many esp system components
set(HAVE_SOCKET 1 CACHE INTERNAL "Have symbol socket" FORCE)

# opendir, closedir and readdir functions are not detected
# but they are available
set(HAVE_OPENDIR 1 CACHE INTERNAL "Have symbol opendir" FORCE)
set(HAVE_CLOSEDIR 1 CACHE INTERNAL "Have symbol closedir" FORCE)
set(HAVE_READDIR 1 CACHE INTERNAL "Have symbol readdir" FORCE)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeModules")

# Disable SMP with esp32 socs that have only one core
if (${IDF_TARGET} MATCHES "esp32s2|esp32c2|esp32c3|esp32c5|esp32c6|esp32c61|esp32h2")
    message("Disabling SMP as selected target only has one core")
    set(AVM_DISABLE_SMP YES FORCE)
    set(HAVE_PLATFORM_ATOMIC_H ON)
endif()

# On Esp32, select is run in a loop in a dedicated task
set(AVM_SELECT_IN_TASK ON)

# JIT is only supported on RISC-V targets (ESP32-C2, ESP32-C3, ESP32-C6, ESP32-H2, ESP32-P4)
# Configuration comes from idf.py menuconfig (KConfig), not CMake options
if(CONFIG_JIT_ENABLED)
    if (${IDF_TARGET} MATCHES "esp32c2|esp32c3|esp32c5|esp32c6|esp32c61|esp32h2|esp32p4")
        set(AVM_DISABLE_JIT OFF)
        set(AVM_JIT_TARGET_ARCH riscv32)
        message(STATUS "JIT compilation enabled for ${IDF_TARGET} (RISC-V32)")
    else()
        message(WARNING "JIT is not supported on ${IDF_TARGET} (Xtensa architecture)")
        set(AVM_DISABLE_JIT ON)
    endif()
else()
    set(AVM_DISABLE_JIT ON)
    message(STATUS "JIT compilation disabled")
endif()

project(atomvm-esp32)

# esp-idf does not use compile_feature but instead sets version in
# c_compile_options
# Ensure project is compiled with at least C11
idf_build_get_property(c_compile_options C_COMPILE_OPTIONS)
if (-std=gnu99 IN_LIST c_compile_options )
    list(REMOVE_ITEM c_compile_options -std=gnu99)
    list(APPEND c_compile_options -std=gnu11)
    idf_build_set_property(C_COMPILE_OPTIONS ${c_compile_options})
endif()

# Options that make sense for this platform
option(AVM_ENABLE_OLD_OTP_VERSIONS "Enable OTP version < 26" OFF)
option(AVM_DISABLE_SMP "Disable SMP." OFF)
option(AVM_USE_32BIT_FLOAT "Use 32 bit floats." OFF)
option(AVM_VERBOSE_ABORT "Print module and line number on VM abort" OFF)
option(AVM_CREATE_STACKTRACES "Create stacktraces" ON)
option(AVM_PRINT_PROCESS_CRASH_DUMPS "Print crash reports when processes die with non-standard reasons" ON)

add_subdirectory(tools)
