#
# This file is part of AtomVM.
#
# Copyright 2018-2020 Davide Bettio <davide@uninstall.it>
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
project (PackBEAM)

set(PACKBEAM_SOURCES
    packbeam.c
)

add_executable(PackBEAM ${PACKBEAM_SOURCES})
target_compile_features(PackBEAM PUBLIC c_std_11)
if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(PackBEAM PUBLIC -Wall -pedantic -Wextra -ggdb)
endif()

find_package(ZLIB)
if (ZLIB_FOUND)
    target_compile_definitions(PackBEAM PUBLIC WITH_ZLIB)
    target_link_libraries(PackBEAM PRIVATE ${ZLIB_LIBRARIES})
endif (ZLIB_FOUND)

if((${CMAKE_SYSTEM_NAME} STREQUAL "Darwin") OR
   (${CMAKE_SYSTEM_NAME} STREQUAL "Linux") OR
   (${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD"))
    target_include_directories(PackBEAM PRIVATE ../../src/platforms/generic_unix/lib)
else()
    message(FATAL_ERROR "Unsupported platform: ${CMAKE_SYSTEM_NAME}")
endif()
set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)
target_link_libraries(PackBEAM PRIVATE libAtomVM${PLATFORM_LIB_SUFFIX})

target_include_directories(PackBEAM PUBLIC ../../src/libAtomVM)
target_link_libraries(PackBEAM PRIVATE libAtomVM)

if (COVERAGE)
    include(CodeCoverage)
    append_coverage_compiler_flags_to_target(PackBEAM)
    append_coverage_linker_flags_to_target(PackBEAM)
endif()
