#
# This file is part of AtomVM.
#
# Copyright 2017-2021 Davide Bettio <davide@uninstall.it>
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
project (tests)

add_executable(test-erlang test.c)
add_executable(test-enif test-enif.c)
add_executable(test-mailbox test-mailbox.c)
add_executable(test-structs test-structs.c)

target_compile_features(test-erlang PUBLIC c_std_11)
target_compile_features(test-enif PUBLIC c_std_11)
target_compile_features(test-mailbox PUBLIC c_std_11)
target_compile_features(test-structs PUBLIC c_std_11)

if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(test-erlang PUBLIC -Wall -pedantic -Wextra -ggdb)
    target_compile_options(test-enif PUBLIC -Wall -pedantic -Wextra -ggdb)
    target_compile_options(test-mailbox PUBLIC -Wall -pedantic -Wextra -ggdb)
    target_compile_options(test-structs PUBLIC -Wall -pedantic -Wextra -ggdb)
endif()

if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    include(CheckFunctionExists)
    include(CheckLibraryExists)
    check_library_exists(rt clock_gettime "time.h" HAVE_CLOCK_GETTIME)
    if (HAVE_CLOCK_GETTIME)
        find_library(LIBRT rt REQUIRED)
        target_link_libraries(test-erlang PRIVATE ${LIBRT})
        target_link_libraries(test-enif PRIVATE ${LIBRT})
        target_link_libraries(test-mailbox PRIVATE ${LIBRT})
        target_link_libraries(test-structs PRIVATE ${LIBRT})
    else()
        # might also be in libc
        check_library_exists(c clock_gettime "" HAVE_CLOCK_GETTIME)
    endif()
endif()

include(MbedTLS)
if (MbedTLS_FOUND)
    target_link_libraries(test-erlang PRIVATE MbedTLS::mbedtls)
    target_link_libraries(test-enif PRIVATE MbedTLS::mbedtls)
    target_link_libraries(test-mailbox PRIVATE MbedTLS::mbedtls)
    target_link_libraries(test-structs PRIVATE MbedTLS::mbedtls)
endif()

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

if((${CMAKE_SYSTEM_NAME} STREQUAL "Darwin") OR
   (${CMAKE_SYSTEM_NAME} STREQUAL "Linux") OR
   (${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD"))
    target_include_directories(test-erlang PRIVATE ../src/platforms/generic_unix/lib)
    target_include_directories(test-enif PRIVATE ../src/platforms/generic_unix/lib)
    target_include_directories(test-mailbox PRIVATE ../src/platforms/generic_unix/lib)
    target_include_directories(test-structs PRIVATE ../src/platforms/generic_unix/lib)
else()
    message(FATAL_ERROR "Unsupported platform: ${CMAKE_SYSTEM_NAME}")
endif()

target_include_directories(test-erlang PRIVATE ../src/libAtomVM)
target_include_directories(test-enif PRIVATE ../src/libAtomVM)
target_include_directories(test-mailbox PRIVATE ../src/libAtomVM)
target_include_directories(test-structs PRIVATE ../src/libAtomVM)
target_link_libraries(test-erlang PRIVATE libAtomVM libAtomVM${PLATFORM_LIB_SUFFIX})
target_link_libraries(test-enif PRIVATE libAtomVM libAtomVM${PLATFORM_LIB_SUFFIX})
target_link_libraries(test-mailbox PRIVATE libAtomVM libAtomVM${PLATFORM_LIB_SUFFIX})
target_link_libraries(test-structs PRIVATE libAtomVM libAtomVM${PLATFORM_LIB_SUFFIX})

# Except for XCode, also compile beams
if (NOT "${CMAKE_GENERATOR}" MATCHES "Xcode")
    add_dependencies(test-erlang erlang_test_modules)
    add_subdirectory(erlang_tests)
    add_subdirectory(libs/estdlib)
    add_subdirectory(libs/eavmlib)
    add_subdirectory(libs/alisp)
endif()

if (COVERAGE)
    include(CodeCoverage)
    append_coverage_compiler_flags_to_target(test-erlang)
    append_coverage_compiler_flags_to_target(test-enif)
    append_coverage_compiler_flags_to_target(test-mailbox)
    append_coverage_compiler_flags_to_target(test-structs)
    append_coverage_linker_flags_to_target(test-erlang)
    append_coverage_linker_flags_to_target(test-enif)
    append_coverage_linker_flags_to_target(test-mailbox)
    append_coverage_linker_flags_to_target(test-structs)
    if (CMAKE_COMPILER_IS_GNUCC)
        setup_target_for_coverage_lcov(NAME coverage EXECUTABLE test-erlang DEPENDENCIES test-erlang)
    endif()
endif()
