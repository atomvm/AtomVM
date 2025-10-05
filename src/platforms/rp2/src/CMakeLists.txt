#
# This file is part of AtomVM.
#
# Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

add_executable(AtomVM main.c)

target_compile_features(AtomVM PUBLIC c_std_11)
if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(AtomVM PUBLIC -ggdb -Os)
    target_compile_options(AtomVM PRIVATE -Wall -pedantic -Wextra)
endif()

# libAtomVM needs to find Pico's platform_smp.h and platform_atomic.h headers
set(HAVE_PLATFORM_SMP_H ON)
# RP2040 needs platform atomic, RP2350 doesn't
if(PICO_RP2040)
set(HAVE_PLATFORM_ATOMIC_H ON)
endif()
add_subdirectory(../../../libAtomVM libAtomVM)
target_link_libraries(AtomVM PUBLIC libAtomVM)
# Also add lib where platform_smp.h and platform_atomic headers are
target_include_directories(libAtomVM PUBLIC lib)

target_link_libraries(AtomVM PUBLIC hardware_regs pico_stdlib pico_binary_info)

if (AVM_REBOOT_ON_NOT_OK)
    target_compile_definitions(AtomVM PRIVATE CONFIG_REBOOT_ON_NOT_OK)
endif()

if (AVM_WAIT_FOR_USB_CONNECT)
    target_compile_definitions(AtomVM PRIVATE PICO_STDIO_USB_CONNECT_WAIT_TIMEOUT_MS=-1)
elseif (AVM_USB_WAIT_SECONDS)
    math(EXPR WAIT_FOR_USB_TIMEOUT "1000 * ${AVM_USB_WAIT_SECONDS}")
    message(STATUS "Wait for USB serial connection timeout ${WAIT_FOR_USB_TIMEOUT} ms")
    target_compile_definitions(AtomVM PRIVATE PICO_STDIO_USB_CONNECT_WAIT_TIMEOUT_MS=${WAIT_FOR_USB_TIMEOUT})
else()
    target_compile_definitions(AtomVM PRIVATE PICO_STDIO_USB_CONNECT_WAIT_TIMEOUT_MS=20000)
endif()

if (AVM_WAIT_BOOTSEL_ON_EXIT)
    target_compile_definitions(AtomVM PRIVATE WAIT_BOOTSEL_ON_EXIT)
endif()

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

add_subdirectory(lib)
target_link_libraries(AtomVM PRIVATE libAtomVM${PLATFORM_LIB_SUFFIX})

# enable usb output, disable uart output
pico_enable_stdio_usb(AtomVM 1)
pico_enable_stdio_uart(AtomVM 0)

# create map/bin/hex/uf2 file in addition to ELF.
pico_add_extra_outputs(AtomVM)
