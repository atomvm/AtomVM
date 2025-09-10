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

cmake_minimum_required (VERSION 3.13)

add_executable(AtomVM main.c)

target_compile_features(AtomVM PUBLIC c_std_11)

add_subdirectory(../../../libAtomVM libAtomVM)
target_link_libraries(AtomVM PUBLIC libAtomVM)
target_compile_options(libAtomVM PUBLIC -O3 -fno-exceptions -fno-rtti -pthread -sINLINING_LIMIT -sUSE_ZLIB=1)
target_compile_definitions(libAtomVM PRIVATE WITH_ZLIB)
target_link_options(AtomVM PRIVATE -sEXPORTED_RUNTIME_METHODS=ccall -sUSE_ZLIB=1 -O3 -pthread -sFETCH -lwebsocket.js --pre-js ${CMAKE_CURRENT_SOURCE_DIR}/atomvm.pre.js)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_link_options(AtomVM PRIVATE -sASSERTIONS=2 -sSAFE_HEAP -sSTACK_OVERFLOW_CHECK)
endif()

if (AVM_EMSCRIPTEN_ENV STREQUAL "web")
    target_link_options(AtomVM PRIVATE -lworkerfs.js -sENVIRONMENT=web,worker -sPROXY_TO_PTHREAD)
else()
    target_link_options(AtomVM PRIVATE -sNODERAWFS -sENVIRONMENT=node)
endif()

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

add_subdirectory(lib)
target_link_libraries(AtomVM PRIVATE libAtomVM${PLATFORM_LIB_SUFFIX})
target_include_directories(AtomVM PRIVATE lib)
