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

project(test_jit)

include(BuildErlang)

set(ERLANG_MODULES
    tests
    jit_tests
    jit_tests_common
    jit_regs_tests
    jit_aarch64_tests
    jit_aarch64_asm_tests
    jit_arm32_tests
    jit_arm32_asm_tests
    jit_armv6m_tests
    jit_armv6m_asm_tests
    jit_armv7m_tests
    jit_armv7m_asm_tests
    jit_riscv32_tests
    jit_riscv32_asm_tests
    jit_riscv64_tests
    jit_riscv64_asm_tests
    jit_wasm32_tests
    jit_wasm32_asm_tests
    jit_x86_64_tests
    jit_x86_64_asm_tests
    jit_xtensa_tests
    jit_xtensa_asm_tests
)

if (NOT AVM_DISABLE_JIT_DWARF)
    pack_archive(test_jit_lib ERLC_FLAGS -DTEST -DJIT_DWARF MODULES jit_dwarf_tests ${ERLANG_MODULES})
else()
    pack_archive(test_jit_lib ERLC_FLAGS -DTEST MODULES ${ERLANG_MODULES})
    # Compile jit_dwarf_tests.beam for BEAM tests even when DWARF is disabled (not packed in .avm)
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/beams/jit_dwarf_tests.beam
        COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/beams
            && erlc +debug_info -DTEST
                -o ${CMAKE_CURRENT_BINARY_DIR}/beams
                -I ${CMAKE_SOURCE_DIR}/libs/include
                -I ${CMAKE_SOURCE_DIR}/libs
                -I ${CMAKE_CURRENT_SOURCE_DIR}/../include
                ${CMAKE_CURRENT_SOURCE_DIR}/jit_dwarf_tests.erl
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/jit_dwarf_tests.erl
        COMMENT "Compiling jit_dwarf_tests.erl (BEAM tests only, not packed in .avm)"
        VERBATIM
    )
    add_custom_target(jit_dwarf_tests_for_beam_tests ALL
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/beams/jit_dwarf_tests.beam
    )
endif()
pack_eunit(test_jit estdlib eavmlib etest jit)
