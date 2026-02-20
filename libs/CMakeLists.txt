#
# This file is part of AtomVM.
#
# Copyright 2018 Fred Dushin <fred@dushin.net>
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

cmake_minimum_required (VERSION 3.12)
project(libs)

add_subdirectory(estdlib/src)
add_subdirectory(eavmlib/src)
add_subdirectory(alisp/src)
add_subdirectory(etest/src)
add_subdirectory(avm_network/src)
add_subdirectory(avm_esp32/src)
add_subdirectory(avm_rp2/src)
add_subdirectory(avm_stm32/src)
add_subdirectory(avm_emscripten/src)
add_subdirectory(esp32boot)
add_subdirectory(esp32devmode/src)
# JIT compiler doesn't compile with OTP < 23
if(Erlang_VERSION VERSION_GREATER_EQUAL "23")
    add_subdirectory(jit/src)
endif()


set(ATOMVM_COMMON_LIBS eavmlib estdlib alisp)

find_package(Elixir)
find_package(Gleam)

if (Elixir_FOUND)
    add_subdirectory(exavmlib/lib)
    list(APPEND ATOMVM_COMMON_LIBS exavmlib)
else()
    message(WARNING "Unable to find elixirc -- skipping Elixir libs")
endif()

if (Gleam_FOUND)
    add_subdirectory(gleam_avm)
    list(APPEND ATOMVM_COMMON_LIBS gleam_avm)
else()
    message(WARNING "Unable to find gleam -- skipping Gleam libs")
endif()

# Base (generic_unix): common + network
pack_lib(atomvmlib ${ATOMVM_COMMON_LIBS} avm_network)

# Platform-specific variants
pack_lib(atomvmlib-esp32 ${ATOMVM_COMMON_LIBS} avm_network avm_esp32)
pack_lib(atomvmlib-rp2 UF2 ${ATOMVM_COMMON_LIBS} avm_network avm_rp2)
pack_lib(atomvmlib-stm32 ${ATOMVM_COMMON_LIBS} avm_stm32)
pack_lib(atomvmlib-emscripten ${ATOMVM_COMMON_LIBS} avm_network avm_emscripten)

if (Dialyzer_FOUND)
    # Helper macro to generate a beams list file from an archive
    macro(dialyzer_beams_list lib_name lib_path)
        get_filename_component(_beams_dir ${lib_path} DIRECTORY)
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${lib_name}_beams.txt
            DEPENDS ${lib_path} PackBEAM
            COMMAND
                ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -l ${lib_path} | sed -e 's|^|${_beams_dir}/beams/|g' > ${CMAKE_CURRENT_BINARY_DIR}/${lib_name}_beams.txt
        )
    endmacro()

    dialyzer_beams_list(estdlib ${CMAKE_CURRENT_BINARY_DIR}/estdlib/src/estdlib.avm)
    dialyzer_beams_list(eavmlib ${CMAKE_CURRENT_BINARY_DIR}/eavmlib/src/eavmlib.avm)
    dialyzer_beams_list(alisp ${CMAKE_CURRENT_BINARY_DIR}/alisp/src/alisp.avm)
    dialyzer_beams_list(avm_network ${CMAKE_CURRENT_BINARY_DIR}/avm_network/src/avm_network.avm)
    dialyzer_beams_list(avm_esp32 ${CMAKE_CURRENT_BINARY_DIR}/avm_esp32/src/avm_esp32.avm)
    dialyzer_beams_list(avm_rp2 ${CMAKE_CURRENT_BINARY_DIR}/avm_rp2/src/avm_rp2.avm)
    dialyzer_beams_list(avm_stm32 ${CMAKE_CURRENT_BINARY_DIR}/avm_stm32/src/avm_stm32.avm)
    dialyzer_beams_list(avm_emscripten ${CMAKE_CURRENT_BINARY_DIR}/avm_emscripten/src/avm_emscripten.avm)

    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/jit_beams.txt
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/jit/src/jit.avm PackBEAM
        COMMAND
            ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -l ${CMAKE_CURRENT_BINARY_DIR}/jit/src/jit.avm | sed -e 's|^|${CMAKE_CURRENT_BINARY_DIR}/jit/src/beams/|g' | grep -v jit_precompile > ${CMAKE_CURRENT_BINARY_DIR}/jit_beams.txt
    )

    # Base beam lists shared by all PLTs
    set(dialyzer_base_lists
            ${CMAKE_CURRENT_BINARY_DIR}/estdlib_beams.txt
            ${CMAKE_CURRENT_BINARY_DIR}/eavmlib_beams.txt
            ${CMAKE_CURRENT_BINARY_DIR}/alisp_beams.txt
            ${CMAKE_CURRENT_BINARY_DIR}/avm_network_beams.txt
    )
    if(Erlang_VERSION VERSION_GREATER_EQUAL "23")
        set(dialyzer_base_lists ${dialyzer_base_lists} ${CMAKE_CURRENT_BINARY_DIR}/jit_beams.txt)
    endif()

    # Helper macro to build a PLT from base lists + platform-specific lists
    macro(build_plt plt_name)
        set(plt_extra_lists ${ARGN})
        set(plt_all_lists ${dialyzer_base_lists} ${plt_extra_lists})
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${plt_name}.plt
            DEPENDS ${plt_all_lists}
            COMMAND cat ${plt_all_lists}
                | xargs dialyzer --build_plt --output_plt ${CMAKE_CURRENT_BINARY_DIR}/${plt_name}.plt
        )
        add_custom_target(${plt_name}_plt
            DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${plt_name}.plt
        )
    endmacro()

    # Base PLT (no platform-specific modules)
    build_plt(atomvmlib)

    # Per-platform PLTs
    build_plt(atomvmlib-esp32 ${CMAKE_CURRENT_BINARY_DIR}/avm_esp32_beams.txt)
    build_plt(atomvmlib-rp2 ${CMAKE_CURRENT_BINARY_DIR}/avm_rp2_beams.txt)
    build_plt(atomvmlib-stm32 ${CMAKE_CURRENT_BINARY_DIR}/avm_stm32_beams.txt)
    build_plt(atomvmlib-emscripten ${CMAKE_CURRENT_BINARY_DIR}/avm_emscripten_beams.txt)
else()
    message("Dialyzer was not found -- skipping PLT build")
endif()

install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/atomvmlib.avm
    DESTINATION lib/atomvm
)
install(
    FILES
        ${CMAKE_CURRENT_BINARY_DIR}/atomvmlib-esp32.avm
        ${CMAKE_CURRENT_BINARY_DIR}/atomvmlib-rp2.avm
        ${CMAKE_CURRENT_BINARY_DIR}/atomvmlib-stm32.avm
        ${CMAKE_CURRENT_BINARY_DIR}/atomvmlib-emscripten.avm
    DESTINATION lib/atomvm
)
if(NOT AVM_DISABLE_JIT)
install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/atomvmlib-${AVM_JIT_TARGET_ARCH}.avm
    DESTINATION lib/atomvm/
)
endif()
