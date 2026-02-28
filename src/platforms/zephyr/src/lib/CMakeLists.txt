#
# This file is part of AtomVM.
#
# Copyright 2022 Paul Guyot <pguyot@kallisys.net>
# Copyright 2023 Winford <winford@object.stream>
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

cmake_minimum_required (VERSION 3.20.5)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)
project (libAtomVM${PLATFORM_LIB_SUFFIX})
zephyr_library_named(libAtomVM${PLATFORM_LIB_SUFFIX})

zephyr_include_directories(
    ${CMAKE_CURRENT_BINARY_DIR}
    ../../../../libAtomVM
    ${CMAKE_BINARY_DIR}/zephyr/include/generated/
)

zephyr_library_sources(
    platform_nifs.c
    sys.c
)

if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC -Wall -pedantic -Wextra -ggdb)
endif()
set_property(TARGET libAtomVM${PLATFORM_LIB_SUFFIX} PROPERTY C_STANDARD 11)

target_link_libraries(app PUBLIC libAtomVM${PLATFORM_LIB_SUFFIX})
target_link_options(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ${LINK_OPTIONS} -Wl,--whole-archive ${CMAKE_CURRENT_BINARY_DIR}/liblibAtomVM${PLATFORM_LIB_SUFFIX}.a -Wl,--no-whole-archive)
