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

add_executable(rp2_tests test_main.c)

if(AVM_RP2_WOKWI_TEST AND NOT PICO_CYW43_SUPPORTED)
    message(FATAL_ERROR "AVM_RP2_WOKWI_TEST requires a WiFi-capable RP2 board")
endif()

if(AVM_RP2_WOKWI_TEST)
    # Leave extra flash space for the Pico W network and TLS firmware.
    set(RP2_TEST_MODULES_ADDRESS 0x10140000)
else()
    set(RP2_TEST_MODULES_ADDRESS 0x10100000)
endif()
target_compile_definitions(rp2_tests PRIVATE AVM_RP2_TEST_MODULES_ADDRESS=${RP2_TEST_MODULES_ADDRESS})

if(PICO_CYW43_SUPPORTED)
    target_compile_definitions(rp2_tests PRIVATE AVM_RP2_SSL_TEST)
endif()

if(AVM_RP2_WOKWI_TEST)
    target_compile_definitions(rp2_tests PRIVATE AVM_RP2_WOKWI_TEST)
endif()

target_compile_features(rp2_tests PUBLIC c_std_11)
if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(rp2_tests PUBLIC -ggdb)
    target_compile_options(rp2_tests PRIVATE -Wall -pedantic -Wextra)
endif()

# libAtomVM needs to find Pico's platform_smp.h and platform_atomic.h headers
set(HAVE_PLATFORM_SMP_H ON)
set(HAVE_PLATFORM_ATOMIC_H ON)
target_link_libraries(rp2_tests PUBLIC libAtomVM)
# Also add lib where platform_smp.h and platform_atomic.h headers are
target_include_directories(rp2_tests PUBLIC ../src/lib)

target_link_libraries(rp2_tests PUBLIC hardware_regs pico_stdlib pico_binary_info unity)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

target_link_libraries(rp2_tests PRIVATE libAtomVM${PLATFORM_LIB_SUFFIX})

# could not get rp2040js to work with stdio USB
pico_enable_stdio_usb(rp2_tests 0)
pico_enable_stdio_uart(rp2_tests 1)

# create map/bin/hex/uf2 file in addition to ELF.
pico_add_extra_outputs(rp2_tests)

add_subdirectory(test_erl_sources)
add_dependencies(rp2_tests rp2_test_modules)

if(AVM_RP2_WOKWI_TEST)
    set(RP2_WOKWI_TESTS_UF2 "${CMAKE_CURRENT_BINARY_DIR}/rp2_wokwi_tests.uf2")
    add_custom_command(
        OUTPUT "${RP2_WOKWI_TESTS_UF2}"
        COMMAND
            "${CMAKE_CURRENT_BINARY_DIR}/test_erl_sources/HostAtomVM-prefix/src/HostAtomVM-build/tools/uf2tool/uf2tool"
            join
            -o "${RP2_WOKWI_TESTS_UF2}"
            "${CMAKE_CURRENT_BINARY_DIR}/rp2_tests.uf2"
            "${CMAKE_CURRENT_BINARY_DIR}/test_erl_sources/rp2_test_modules.uf2"
        DEPENDS
            rp2_tests
            rp2_test_modules
            "${CMAKE_CURRENT_BINARY_DIR}/test_erl_sources/rp2_test_modules.uf2"
        COMMENT "Combining RP2 Wokwi test firmware"
        VERBATIM
    )
    add_custom_target(rp2_wokwi_tests DEPENDS "${RP2_WOKWI_TESTS_UF2}")
    set_property(
        DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${RP2_WOKWI_TESTS_UF2}"
    )
endif()
