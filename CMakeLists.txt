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
find_package(Elixir)

option(AVM_DISABLE_FP "Disable floating point support." OFF)
option(AVM_DISABLE_SMP "Disable SMP." OFF)
option(AVM_DISABLE_TASK_DRIVER "Disable task driver support." OFF)
option(AVM_USE_32BIT_FLOAT "Use 32 bit floats." OFF)
option(AVM_VERBOSE_ABORT "Print module and line number on VM abort" OFF)
option(AVM_RELEASE "Build an AtomVM release" OFF)
option(AVM_CREATE_STACKTRACES "Create stacktraces" ON)
option(AVM_BUILD_RUNTIME_ONLY "Only build the AtomVM runtime" OFF)
option(COVERAGE "Build for code coverage" OFF)

if((${CMAKE_SYSTEM_NAME} STREQUAL "Darwin") OR
   (${CMAKE_SYSTEM_NAME} STREQUAL "Linux") OR
   (${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD"))
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
