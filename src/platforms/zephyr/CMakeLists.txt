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

set(ATOMVM_ZEPHYR_ROOT ${CMAKE_CURRENT_SOURCE_DIR})
include(${ATOMVM_ZEPHYR_ROOT}/cmake/AtomVMZephyrApp.cmake)
