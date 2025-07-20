#
# This file is part of AtomVM.
#
# Copyright 2017-2019 Davide Bettio <davide@uninstall.it>
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
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/CMakeModules)

find_package(Dialyzer)
find_package(Erlang)
find_package(Elixir)
find_package(Gleam)

option(AVM_DISABLE_FP "Disable floating point support." OFF)
option(AVM_DISABLE_SMP "Disable SMP." OFF)
option(AVM_DISABLE_TASK_DRIVER "Disable task driver support." OFF)
option(AVM_DISABLE_JIT "Disable just in time compilation." ON)
option(AVM_ENABLE_PRECOMPILED "Enable execution of precompiled code, even if JIT is disabled." OFF)
option(AVM_USE_32BIT_FLOAT "Use 32 bit floats." OFF)
option(AVM_VERBOSE_ABORT "Print module and line number on VM abort" OFF)
option(AVM_RELEASE "Build an AtomVM release" OFF)
option(AVM_CREATE_STACKTRACES "Create stacktraces" ON)
option(AVM_BUILD_RUNTIME_ONLY "Only build the AtomVM runtime" OFF)
option(COVERAGE "Build for code coverage" OFF)

# JIT & execution of precompiled code
if (CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
    if (NOT AVM_DISABLE_JIT)
        set(AVM_JIT_TARGET_ARCH ${CMAKE_SYSTEM_PROCESSOR})
    endif()
else()
    if (NOT AVM_DISABLE_JIT)
        message("JIT is not supported on ${CMAKE_SYSTEM_PROCESSOR}")
        set(AVM_DISABLE_JIT ON CACHE BOOL "Disable just in time compilation." FORCE)
        set(AVM_ENABLE_PRECOMPILED OFF CACHE BOOL "Enable execution of precompiled code, even if JIT is disabled." FORCE)
    endif()
endif()

if((${CMAKE_SYSTEM_NAME} STREQUAL "Darwin") OR
   (${CMAKE_SYSTEM_NAME} STREQUAL "Linux") OR
   (${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD") OR
   (${CMAKE_SYSTEM_NAME} STREQUAL "DragonFly"))
    add_subdirectory(src/platforms/generic_unix)
else()
    message(FATAL_ERROR "Unsupported platform: ${CMAKE_SYSTEM_NAME}")
endif()

if(NOT AVM_BUILD_RUNTIME_ONLY)
    add_subdirectory(tests)
endif()

add_subdirectory(tools/packbeam)
add_subdirectory(tools/uf2tool)

if (NOT "${CMAKE_GENERATOR}" MATCHES "Xcode")
    add_custom_target(dialyzer COMMENT "Run dialyzer")
    add_subdirectory(libs)
    if(NOT AVM_BUILD_RUNTIME_ONLY)
        add_subdirectory(examples)
        add_subdirectory(doc)
    endif()
endif()
