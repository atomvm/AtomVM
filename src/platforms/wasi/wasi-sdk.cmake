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

# CMake toolchain file for WASI SDK
# Usage: cmake -DCMAKE_TOOLCHAIN_FILE=/path/to/wasi-sdk.cmake ..

if(DEFINED ENV{WASI_SDK_PATH})
    set(WASI_SDK_PATH $ENV{WASI_SDK_PATH})
elseif(EXISTS /opt/local/libexec/wasi-sdk)
    set(WASI_SDK_PATH /opt/local/libexec/wasi-sdk)
elseif(EXISTS /opt/wasi-sdk)
    set(WASI_SDK_PATH /opt/wasi-sdk)
else()
    message(FATAL_ERROR "WASI SDK not found. Install via MacPorts: sudo port install wasi-sdk, or set WASI_SDK_PATH.")
endif()

set(CMAKE_SYSTEM_NAME WASI)
set(CMAKE_SYSTEM_PROCESSOR wasm32)

set(CMAKE_C_COMPILER ${WASI_SDK_PATH}/bin/clang)
set(CMAKE_CXX_COMPILER ${WASI_SDK_PATH}/bin/clang++)
set(CMAKE_AR ${WASI_SDK_PATH}/bin/llvm-ar)
set(CMAKE_RANLIB ${WASI_SDK_PATH}/bin/llvm-ranlib)
set(CMAKE_STRIP ${WASI_SDK_PATH}/bin/llvm-strip)

# Pick the WASI target triple. The toolchain file is re-executed inside
# try_compile() projects, where our parent project's cache (AVM_DISABLE_SMP,
# WASI_TARGET) is NOT inherited - but CMAKE_C_COMPILER_TARGET IS, via
# CMakeCCompiler.cmake. Honour that first so try_compile() invocations don't
# silently drift to a different target. (The matrix of WASI targets vs SMP /
# networking is documented in src/platforms/wasi/CMakeLists.txt.)
if(CMAKE_C_COMPILER_TARGET)
    set(WASI_TARGET "${CMAKE_C_COMPILER_TARGET}")
elseif(NOT DEFINED WASI_TARGET)
    if(AVM_DISABLE_SMP)
        set(WASI_TARGET "wasm32-wasip2" CACHE STRING "WASI target triple")
    else()
        set(WASI_TARGET "wasm32-wasip1-threads" CACHE STRING "WASI target triple")
    endif()
endif()

set(CMAKE_C_COMPILER_TARGET ${WASI_TARGET})
set(CMAKE_CXX_COMPILER_TARGET ${WASI_TARGET})

set(CMAKE_SYSROOT ${WASI_SDK_PATH}/share/wasi-sysroot)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)

set(CMAKE_EXECUTABLE_SUFFIX ".wasm")

message(STATUS "WASI SDK: ${WASI_SDK_PATH}, target: ${WASI_TARGET}")
