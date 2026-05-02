#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
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
set_target_properties(AtomVM PROPERTIES SUFFIX ".wasm")

target_compile_features(AtomVM PUBLIC c_std_11)

add_subdirectory(../../../libAtomVM libAtomVM)
target_link_libraries(AtomVM PUBLIC libAtomVM)

# Force-include the WASI compat shim (tzset prototype, missing sockopts) into
# every libAtomVM compile unit and into the WASI platform lib too. The platform
# lib pulls in libAtomVM/otp_socket.c, which expects SO_LINGER.
set(WASI_COMPAT_INCLUDE "-include${CMAKE_CURRENT_SOURCE_DIR}/lib/wasi_compat.h")
target_compile_options(libAtomVM PRIVATE ${WASI_COMPAT_INCLUDE})

if (NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(libAtomVM PUBLIC -O3)
endif()

# Export malloc/free so the embedding host can pass binaries into the wasm
# instance without each host having to wrap them itself.
set(WASI_LINK_OPTIONS
    -O3
    -Wl,--initial-memory=67108864
    -Wl,--max-memory=268435456
    -Wl,--export-dynamic
    -Wl,--export=malloc
    -Wl,--export=free
)

if (NOT AVM_DISABLE_SMP)
    list(APPEND WASI_LINK_OPTIONS -pthread)
    target_compile_options(libAtomVM PUBLIC -pthread)
endif()

target_link_options(AtomVM PRIVATE ${WASI_LINK_OPTIONS})

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(AtomVM PRIVATE -g)
    target_link_options(AtomVM PRIVATE -g)
    target_compile_options(libAtomVM PRIVATE -g)
endif()

set(PLATFORM_LIB_SUFFIX ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR})

add_subdirectory(lib)
target_link_libraries(AtomVM PRIVATE libAtomVM${PLATFORM_LIB_SUFFIX})
target_include_directories(AtomVM PRIVATE lib)
