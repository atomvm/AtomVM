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

ExternalProject_Add(HostAtomVM
    SOURCE_DIR ../../../../../../
    INSTALL_COMMAND cmake -E echo "Skipping install step."
    BUILD_COMMAND cmake --build . --target=atomvmlib --target=PackBEAM
)

function(compile_erlang module_name module_src_dir)
    add_custom_command(
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.beam"
        COMMAND erlc ${CMAKE_CURRENT_SOURCE_DIR}/${module_src_dir}/${module_name}.erl
        DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${module_src_dir}/${module_name}.erl"
        COMMENT "Compiling ${module_name}.erl"
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )

    set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.beam")
endfunction()

compile_erlang(test_clocks "")
compile_erlang(test_smp "")
compile_erlang(test_crypto ../../../esp32/test/main/test_erl_sources/)

add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/rp2040_test_modules.avm"
    COMMAND HostAtomVM-prefix/src/HostAtomVM-build/tools/packbeam/PackBEAM -i rp2040_test_modules.avm
        HostAtomVM-prefix/src/HostAtomVM-build/libs/atomvmlib.avm
        test_clocks.beam
        test_smp.beam
        test_crypto.beam
    DEPENDS
        HostAtomVM
        "${CMAKE_CURRENT_BINARY_DIR}/test_clocks.beam"
        "${CMAKE_CURRENT_BINARY_DIR}/test_smp.beam"
        "${CMAKE_CURRENT_BINARY_DIR}/test_crypto.beam"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    VERBATIM
)

set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/rp2040_test_modules.avm")

add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/rp2040_test_modules.uf2"
    COMMAND HostAtomVM-prefix/src/HostAtomVM-build/tools/uf2tool/uf2tool
        create
        -o rp2040_test_modules.uf2
        -s 0x10100000 "${CMAKE_CURRENT_BINARY_DIR}/rp2040_test_modules.avm"
    DEPENDS
        HostAtomVM
        "${CMAKE_CURRENT_BINARY_DIR}/rp2040_test_modules.avm"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    VERBATIM
)

set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/rp2040_test_modules.uf2")

add_custom_target(rp2040_test_modules DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/rp2040_test_modules.uf2")
