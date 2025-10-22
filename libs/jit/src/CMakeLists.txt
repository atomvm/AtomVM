#
# This file is part of AtomVM.
#
# Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

project(jit)

include(BuildErlang)

set(ERLANG_MODULES
    jit
    jit_precompile
    jit_stream_binary
    jit_stream_mmap
    jit_aarch64
    jit_aarch64_asm
    jit_armv6m
    jit_armv6m_asm
    jit_x86_64
    jit_x86_64_asm
)

pack_precompiled_archive(jit ${ERLANG_MODULES})

include(../../../version.cmake)

set(JIT_VERSION ${ATOMVM_BASE_VERSION})

install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/beams/
    DESTINATION lib/atomvm/lib/jit-${JIT_VERSION}/ebin
    FILES_MATCHING PATTERN "*.beam"
)

install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/jit.avm
    DESTINATION lib/atomvm/lib/jit-${JIT_VERSION}/ebin/
)

install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/
    DESTINATION lib/atomvm/lib/jit-${JIT_VERSION}/src
    FILES_MATCHING PATTERN "*.erl"
)

install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../include
    DESTINATION lib/atomvm/lib/jit-${JIT_VERSION}/include
    FILES_MATCHING PATTERN "*.hrl"
)
