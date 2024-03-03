#
# This file is part of AtomVM.
#
# Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

cmake_minimum_required (VERSION 3.14)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeModules")
include(FetchUnity)

add_executable(rp2040_tests test_main.c)

target_compile_features(rp2040_tests PUBLIC c_std_11)
if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(rp2040_tests PUBLIC -ggdb)
    target_compile_options(rp2040_tests PRIVATE -Wall -pedantic -Wextra)
endif()

# libAtomVM needs to find Pico's platform_smp.h and platform_atomic.h headers
set(HAVE_PLATFORM_SMP_H ON)
set(HAVE_PLATFORM_ATOMIC_H ON)
target_link_libraries(rp2040_tests PUBLIC libAtomVM)
# Also add lib where platform_smp.h and platform_atomic.h headers are
target_include_directories(rp2040_tests PUBLIC ../src/lib)

target_link_libraries(rp2040_tests PUBLIC hardware_regs pico_stdlib pico_binary_info unity)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

target_link_libraries(rp2040_tests PRIVATE libAtomVM${PLATFORM_LIB_SUFFIX})

# could not get rp2040js to work with stdio USB
pico_enable_stdio_usb(rp2040_tests 0)
pico_enable_stdio_uart(rp2040_tests 1)

# create map/bin/hex/uf2 file in addition to ELF.
pico_add_extra_outputs(rp2040_tests)

add_subdirectory(test_erl_sources)
add_dependencies(rp2040_tests rp2040_test_modules)
