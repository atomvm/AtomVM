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
    jit_dwarf_pt
    jit
    jit_dwarf
    jit_precompile
    jit_regs
    jit_stream_binary
    jit_stream_flash
    jit_stream_mmap
    jit_aarch64
    jit_aarch64_asm
    jit_armv6m
    jit_armv6m_asm
    jit_riscv32
    jit_riscv32_asm
    jit_riscv64
    jit_riscv64_asm
    jit_x86_64
    jit_x86_64_asm
)

include(../../../version.cmake)

if (NOT AVM_DISABLE_JIT_DWARF)
    set(erlc_flags -DJIT_DWARF -DATOMVM_VERSION=\"${ATOMVM_BASE_VERSION}\" -pa ${CMAKE_CURRENT_BINARY_DIR}/beams)
else()
    set(erlc_flags)
endif()

pack_precompiled_archive(jit ERLC_FLAGS ${erlc_flags} MODULES ${ERLANG_MODULES})

if (NOT AVM_DISABLE_JIT_DWARF)
    # jit.beam needs jit_dwarf_pt.beam compiled first (parse transform dependency)
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/beams/jit.beam
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/beams/jit_dwarf_pt.beam
        APPEND
    )
endif()

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
