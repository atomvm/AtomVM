#
# This file is part of AtomVM.
#
# Copyright 2026 Peter M. <petermm@gmail.com>
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
project(libAtomVMPlatformRTEMS)

set(HEADER_FILES
    rtems_sys.h
    ../../../../libAtomVM/platform_nifs.h
    ../../../../libAtomVM/portnifloader.h
    ../../../../libAtomVM/sys.h
)

set(SOURCE_FILES
    platform_nifs.c
    sys.c
    ../../../../libAtomVM/portnifloader.c
)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

add_library(libAtomVM${PLATFORM_LIB_SUFFIX} ${SOURCE_FILES} ${HEADER_FILES})
target_compile_features(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC c_std_11)
if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC -Wall -pedantic -Wextra -ggdb)
endif()

target_include_directories(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC libAtomVM)
