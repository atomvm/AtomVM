#
# This file is part of AtomVM.
#
# Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

set(PICO_SDK_FETCH_FROM_GIT on)

# pico_sdk_import.cmake is a single file copied from this SDK
# note: this must happen before project()
include(pico_sdk_import.cmake)

project(AtomVM)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeModules")

# initialize the Raspberry Pi Pico SDK
pico_sdk_init()

# Pico SDK forces compiler, but we really want to know its features.
# TODO: avoid using private api: https://discourse.cmake.org/t/cmakedeterminecompilerfeatures-cmake-is-no-more-in-3-30-bug-or-not/11176/3
if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.30.0)
    include(${CMAKE_ROOT}/Modules/CMakeDetermineCompilerSupport.cmake)
    cmake_determine_compiler_support(C)
else()
    include(${CMAKE_ROOT}/Modules/CMakeDetermineCompileFeatures.cmake)
    CMAKE_DETERMINE_COMPILE_FEATURES(C)
endif()

enable_language( C CXX ASM )

# Pico SDK currently forces -nostdlib when using TRY_COMPILE
# So we don't try to link.
set(CMAKE_TRY_COMPILE_TARGET_TYPE "STATIC_LIBRARY")

# mkfifo is defined in newlib header but not implemented
set(HAVE_MKFIFO "" CACHE INTERNAL "Have symbol mkfifo" FORCE)
# also avoid exposing unlink to silence warning that it will always fail
set(HAVE_UNLINK "" CACHE INTERNAL "Have symbol unlink" FORCE)
# Likewise with EXECVE
set(HAVE_EXECVE "" CACHE INTERNAL "Have symbol execve" FORCE)

# Options that make sense for this platform
option(AVM_DISABLE_SMP "Disable SMP support." OFF)
option(AVM_USE_32BIT_FLOAT "Use 32 bit floats." OFF)
option(AVM_VERBOSE_ABORT "Print module and line number on VM abort" OFF)
option(AVM_WAIT_FOR_USB_CONNECT "Wait for USB connection before starting" OFF)
option(AVM_WAIT_BOOTSEL_ON_EXIT "Wait in BOOTSEL rather than shutdown on exit" ON)
option(AVM_REBOOT_ON_NOT_OK "Reboot Pico if result is not ok" OFF)
option(AVM_CREATE_STACKTRACES "Create stacktraces" ON)
option(AVM_DISABLE_JIT "Disable just in time compilation." ON)
if(CMAKE_SYSTEM_PROCESSOR MATCHES "^cortex-m.+$")
    # We only have armv6m for now, which all cortex-m should support
    if (NOT AVM_DISABLE_JIT)
        set(AVM_JIT_TARGET_ARCH "armv6m")
    endif()
else()
    # Typically riscv is not supported yet
    if (NOT AVM_DISABLE_JIT)
        message("JIT is not supported on ${CMAKE_SYSTEM_PROCESSOR}")
        set(AVM_DISABLE_JIT ON CACHE BOOL "Disable just in time compilation." FORCE)
        set(AVM_ENABLE_PRECOMPILED OFF CACHE BOOL "Enable execution of precompiled code, even if JIT is disabled." FORCE)
    endif()
endif()

set(AVM_DISABLE_TASK_DRIVER ON FORCE)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

add_subdirectory(src)
add_subdirectory(tests)
