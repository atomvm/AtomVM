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
project (libAtomVMPlatformSTM32)

set(HEADER_FILES
    avm_devcfg.h
    avm_log.h
    gpio_driver.h
    stm_sys.h
    ../../../../libAtomVM/platform_nifs.h
    ../../../../libAtomVM/sys.h
)

set(SOURCE_FILES
    gpio_driver.c
    platform_nifs.c
    sys.c
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

target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC libAtomVM)
target_link_options(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ${LINK_OPTIONS} -Wl,--whole-archive ${CMAKE_CURRENT_BINARY_DIR}/liblibAtomVMGeneric-arm.a -Wl,--no-whole-archive)

target_include_directories(libAtomVM${PLATFORM_LIB_SUFFIX} SYSTEM PUBLIC ${OPENCM3_INCLUDE})
target_link_directories(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ${OPENCM3_LIB})
target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ${OPENCM3_RUNTIME})
