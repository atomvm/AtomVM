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

cmake_minimum_required (VERSION 2.6)

project(AtomVM)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/CMakeModules)

find_package(Elixir)

option(AVM_DISABLE_FP "Disable floating point support." OFF)
if (AVM_DISABLE_FP)
    add_definitions(-DAVM_NO_FP)
endif()

option(AVM_USE_32BIT_FLOAT "Use 32 bit floats." OFF)
if (AVM_USE_32BIT_FLOAT)
    add_definitions(-DAVM_USE_SINGLE_PRECISION)
endif()

option(AVM_VERBOSE_ABORT "Print module and line number on VM abort" OFF)
if (AVM_VERBOSE_ABORT)
    add_definitions(-DAVM_VERBOSE_ABORT)
endif()

add_subdirectory(src)
add_subdirectory(tests)
add_subdirectory(tools/packbeam)
add_subdirectory(tools/release/esp32)
if (NOT "${CMAKE_GENERATOR}" MATCHES "Xcode")
    add_subdirectory(libs)
    add_subdirectory(examples)
    add_subdirectory(doc)
endif()

if (CMAKE_BUILD_TYPE STREQUAL "Coverage")
    include(CodeCoverage)
    SETUP_TARGET_FOR_COVERAGE(NAME coverage EXECUTABLE test-erlang DEPENDENCIES test-erlang)
endif()
