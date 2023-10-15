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

cmake_minimum_required(VERSION 3.16)

set(EXTRA_COMPONENT_DIRS "../components")

# Set the components to include the tests for.
# This can be overriden from CMake cache:
# - when invoking CMake directly: cmake -D TEST_COMPONENTS="xxxxx" ..
# - when using idf.py: idf.py -T xxxxx build
#
set(TEST_COMPONENTS "testable" CACHE STRING "List of components to test")

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeModules")

include($ENV{IDF_PATH}/tools/cmake/project.cmake)

# Don't try to link when testing availability of functions
set(CMAKE_TRY_COMPILE_TARGET_TYPE "STATIC_LIBRARY")

# mkfifo is defined in newlib header but not implemented
set(HAVE_MKFIFO NO)
set(HAVE_MKFIFO "" CACHE INTERNAL "Have symbol mkfifo" FORCE)
# Force HAVE_SOCKET
# Automatically detecting it requires to put too many components include dirs
# in CMAKE_REQUIRED_INCLUDES as lwip includes freetos and many esp system components
set(HAVE_SOCKET ON)
set(HAVE_SOCKET 1 CACHE INTERNAL "Have symbol socket" FORCE)

# Disable SMP with esp32 socs that have only one core
if (${IDF_TARGET} MATCHES "esp32s2|esp32c3|esp32c6|esp32h2")
    message("Disabling SMP as selected target only has one core")
    set(AVM_DISABLE_SMP YES FORCE)
    set(HAVE_PLATFORM_ATOMIC_H ON)
endif()

project(atomvm-esp32-test)

# esp-idf does not use compile_feature but instead sets version in
# c_compile_options
# Ensure project is compiled with at least C11
idf_build_get_property(c_compile_options C_COMPILE_OPTIONS)
if (-std=gnu99 IN_LIST c_compile_options )
    list(REMOVE_ITEM c_compile_options -std=gnu99)
    list(APPEND c_compile_options -std=gnu11)
    idf_build_set_property(C_COMPILE_OPTIONS ${c_compile_options})
endif()
