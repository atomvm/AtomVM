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

include(ExternalProject)
if(NOT AVM_DISABLE_JIT)
set(host_atomvm_jit_target "--target=jit")
else()
set(host_atomvm_jit_target "")
endif()
ExternalProject_Add(HostAtomVM
    SOURCE_DIR ../../../../../../
    INSTALL_COMMAND cmake -E echo "Skipping install step."
    BUILD_COMMAND cmake --build . --target=atomvmlib ${host_atomvm_jit_target} --target=PackBEAM --target=UF2Tool
)

macro(jit_precompile module_name)
    if(NOT AVM_DISABLE_JIT)
        add_custom_command(
            OUTPUT ${AVM_JIT_TARGET_ARCH}/${module_name}.beam
            COMMAND mkdir -p ${AVM_JIT_TARGET_ARCH}
                && erl -pa HostAtomVM-prefix/src/HostAtomVM-build/libs/jit/src/beams/ -noshell -s jit_precompile -s init stop -- ${AVM_JIT_TARGET_ARCH} ${AVM_JIT_TARGET_ARCH}/ ${module_name}.beam
            DEPENDS ${module_name}.beam HostAtomVM
            COMMENT "Compiling ${module_name}.beam to ${AVM_JIT_TARGET_ARCH}"
            VERBATIM
        )
    endif()
endmacro()

function(compile_erlang module_name module_src_dir)
    add_custom_command(
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.beam"
        COMMAND erlc ${CMAKE_CURRENT_SOURCE_DIR}/${module_src_dir}/${module_name}.erl
        DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${module_src_dir}/${module_name}.erl"
        COMMENT "Compiling ${module_name}.erl"
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
    jit_precompile(${module_name})

    set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.beam")
endfunction()

compile_erlang(test_clocks "")
compile_erlang(test_smp "")
compile_erlang(test_crypto ../../../esp32/test/main/test_erl_sources/)

set(erlang_test_beams
    test_clocks.beam
    test_smp.beam
    test_crypto.beam
)

if(NOT AVM_DISABLE_JIT)
    set(erlang_test_beams_${AVM_JIT_TARGET_ARCH} ${erlang_test_beams})
    list(TRANSFORM erlang_test_beams_${AVM_JIT_TARGET_ARCH} PREPEND ${AVM_JIT_TARGET_ARCH}/)
    list(APPEND erlang_test_beams ${erlang_test_beams_${AVM_JIT_TARGET_ARCH}})
endif()

add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/rp2_test_modules.avm"
    COMMAND HostAtomVM-prefix/src/HostAtomVM-build/tools/packbeam/PackBEAM -i rp2_test_modules.avm
        HostAtomVM-prefix/src/HostAtomVM-build/libs/atomvmlib.avm
        ${erlang_test_beams}
    DEPENDS
        HostAtomVM
        ${erlang_test_beams}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    VERBATIM
)

set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/rp2_test_modules.avm")

add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/rp2_test_modules.uf2"
    COMMAND HostAtomVM-prefix/src/HostAtomVM-build/tools/uf2tool/uf2tool
        create
        -o rp2_test_modules.uf2
        -s 0x10100000 "${CMAKE_CURRENT_BINARY_DIR}/rp2_test_modules.avm"
    DEPENDS
        HostAtomVM
        "${CMAKE_CURRENT_BINARY_DIR}/rp2_test_modules.avm"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    VERBATIM
)

set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/rp2_test_modules.uf2")

add_custom_target(rp2_test_modules DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/rp2_test_modules.uf2")
