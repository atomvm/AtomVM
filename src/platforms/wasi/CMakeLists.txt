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
cmake_minimum_required(VERSION 3.13)
project(AtomVM)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeModules")

# Require WASM toolchain
if (NOT CMAKE_SYSTEM_NAME STREQUAL "WASI")
    message(FATAL_ERROR "This platform requires WASI toolchain. Use -DCMAKE_TOOLCHAIN_FILE=/path/to/wasi-sdk.cmake")
endif()

# Options that make sense for this platform
option(AVM_DISABLE_SMP "Disable SMP." OFF)
option(AVM_USE_32BIT_FLOAT "Use 32 bit floats." OFF)
option(AVM_VERBOSE_ABORT "Print module and line number on VM abort" OFF)
option(AVM_CREATE_STACKTRACES "Create stacktraces" ON)

# WASI target selection. SMP and networking are currently mutually exclusive:
# - wasm32-wasip1            : no SMP, no networking
# - wasm32-wasip1-threads    : SMP, no networking
# - wasm32-wasip2            : networking via wasi:sockets@0.2.x, no SMP
#
# The default mirrors the AVM_DISABLE_SMP toggle: SMP-enabled builds (the
# project default) pick wasip1-threads (no networking), SMP-disabled builds
# pick wasip2 (networking). Override with -DWASI_TARGET=... to pick wasip1
# explicitly or to pin a specific triple from a CI matrix.
if (NOT DEFINED WASI_TARGET)
    if (AVM_DISABLE_SMP)
        set(WASI_TARGET "wasm32-wasip2" CACHE STRING "WASI target triple")
    else()
        set(WASI_TARGET "wasm32-wasip1-threads" CACHE STRING "WASI target triple")
    endif()
endif()

# WASI 0.2 / Preview 2 detection - used only for diagnostics. The actual
# networking codepath is gated on the compiler-defined __wasip2__ macro.
if (WASI_TARGET MATCHES "wasip2")
    set(WASI_PREVIEW 2)
    message(STATUS "WASI Preview 2 selected (networking via wasi:sockets@0.2.x): ${WASI_TARGET}")
    if (NOT AVM_DISABLE_SMP)
        message(FATAL_ERROR
            "wasm32-wasip2 does not support threads. Either build with "
            "-DAVM_DISABLE_SMP=ON or pick wasm32-wasip1-threads (no networking).")
    endif()
else()
    set(WASI_PREVIEW 1)
    message(STATUS "WASI Preview 1 selected (no networking): ${WASI_TARGET}")
endif()

# Surface the actual WASI target through erlang:system_info(system_architecture)
# so tests can branch on it (e.g. suffix-match "wasip2" to gate networking).
# WASI has no vendor concept, so we keep the bare 2-part form.
set(AVM_SYSTEM_ARCHITECTURE_STRING "${WASI_TARGET}" CACHE STRING "" FORCE)

# Ensure we're using the correct compiler target
set(CMAKE_C_COMPILER_TARGET ${WASI_TARGET})
set(CMAKE_CXX_COMPILER_TARGET ${WASI_TARGET})

set(PLATFORM_LIB_SUFFIX ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR})

add_subdirectory(src)
