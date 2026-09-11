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
add_subdirectory(avm_unix/src)
add_subdirectory(esp32boot)
add_subdirectory(esp32devmode/src)
add_subdirectory(jit/src)


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

# Base (generic_unix): common + network + unix — wasm32 is emscripten-only
set(AVM_NATIVE_PRECOMPILED_TARGETS ${AVM_PRECOMPILED_TARGETS})
list(REMOVE_ITEM AVM_NATIVE_PRECOMPILED_TARGETS wasm32 armv6m+thumb2+float32 xtensa+float32)
pack_lib(atomvmlib ${ATOMVM_COMMON_LIBS} avm_network avm_unix TARGETS ${AVM_NATIVE_PRECOMPILED_TARGETS})

# Platform-specific variants
pack_lib(atomvmlib-esp32 ${ATOMVM_COMMON_LIBS} avm_network avm_esp32 TARGETS xtensa xtensa+float32 riscv32)
pack_lib(atomvmlib-rp2 UF2 ${ATOMVM_COMMON_LIBS} avm_network avm_rp2 TARGETS armv6m armv6m+thumb2 armv6m+thumb2+float32 riscv32)
pack_lib(atomvmlib-stm32 ${ATOMVM_COMMON_LIBS} avm_stm32 TARGETS armv6m armv6m+thumb2 armv6m+thumb2+float32)
pack_lib(atomvmlib-emscripten ${ATOMVM_COMMON_LIBS} avm_network avm_emscripten TARGETS wasm32)

if (Dialyzer_FOUND)
    # Helper macro to generate a beams list file from an archive
    macro(dialyzer_beams_list lib_name lib_path)
        get_filename_component(_beams_dir ${lib_path} DIRECTORY)
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${lib_name}_beams.txt
            DEPENDS ${lib_path} PackBEAM
            COMMAND
                ${CMAKE_BINARY_DIR}/tools/packbeam/packbeam list -f bare ${lib_path} | sed -e 's|^|${_beams_dir}/beams/|g' > ${CMAKE_CURRENT_BINARY_DIR}/${lib_name}_beams.txt
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
            ${CMAKE_BINARY_DIR}/tools/packbeam/packbeam list -f bare ${CMAKE_CURRENT_BINARY_DIR}/jit/src/jit.avm | sed -e 's|^|${CMAKE_CURRENT_BINARY_DIR}/jit/src/beams/|g' | grep -v jit_precompile > ${CMAKE_CURRENT_BINARY_DIR}/jit_beams.txt
    )

    # Base beam lists shared by all PLTs
    set(dialyzer_base_lists
            ${CMAKE_CURRENT_BINARY_DIR}/estdlib_beams.txt
            ${CMAKE_CURRENT_BINARY_DIR}/eavmlib_beams.txt
            ${CMAKE_CURRENT_BINARY_DIR}/alisp_beams.txt
            ${CMAKE_CURRENT_BINARY_DIR}/avm_network_beams.txt
    )
    set(dialyzer_base_lists ${dialyzer_base_lists} ${CMAKE_CURRENT_BINARY_DIR}/jit_beams.txt)

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

# Supported API: the functions AtomVM makes callable, and the check that keeps
# the list complete.
set(SUPPORTED_API_TOOLS ${CMAKE_SOURCE_DIR}/tools/supported_api)
set(SUPPORTED_API_DIR ${CMAKE_BINARY_DIR}/supported_api)

# The module list comes from the archive rather than from the beams directory,
# which also holds the precompiled beams of every JIT target and, for some
# libraries, beams that are not packed.
macro(supported_api_fragment lib_name)
    if(${lib_name} STREQUAL "exavmlib")
        set(_sa_avm ${CMAKE_CURRENT_BINARY_DIR}/${lib_name}/lib/${lib_name}.avm)
        set(_sa_beams ${CMAKE_CURRENT_BINARY_DIR}/${lib_name}/lib/beams)
    elseif(${lib_name} STREQUAL "gleam_avm")
        set(_sa_avm ${CMAKE_CURRENT_BINARY_DIR}/${lib_name}/${lib_name}.avm)
        set(_sa_beams ${CMAKE_CURRENT_BINARY_DIR}/${lib_name}/build/prod/erlang/${lib_name}/ebin)
    else()
        set(_sa_avm ${CMAKE_CURRENT_BINARY_DIR}/${lib_name}/src/${lib_name}.avm)
        set(_sa_beams ${CMAKE_CURRENT_BINARY_DIR}/${lib_name}/src/beams)
    endif()
    get_filename_component(_sa_dir ${_sa_avm} DIRECTORY)
    add_custom_command(
        OUTPUT ${_sa_dir}/${lib_name}.funcs
        DEPENDS ${_sa_avm} PackBEAM ${SUPPORTED_API_TOOLS}/extract_beam_exports.erl
        COMMAND
            ${CMAKE_BINARY_DIR}/tools/packbeam/packbeam list -f bare ${_sa_avm}
                | sed -e 's|^|${_sa_beams}/|g' > ${_sa_dir}/${lib_name}.beams
        COMMAND
            escript ${SUPPORTED_API_TOOLS}/extract_beam_exports.erl
                -o ${_sa_dir}/${lib_name}.funcs --files-from ${_sa_dir}/${lib_name}.beams
        COMMENT "Generating exported function list of ${lib_name}"
    )
    set(SUPPORTED_API_${lib_name}_FUNCS ${_sa_dir}/${lib_name}.funcs)
endmacro()

# Every library that ends up in an atomvmlib archive. jit is left out of the
# contract, its modules being the compiler itself, but it takes part in the
# check because the gperf tables register jit:backend_module/0 and
# jit:stream_module/0, whose Erlang home is libs/jit.
set(SUPPORTED_API_CONTRACT_LIBS ${ATOMVM_COMMON_LIBS} etest avm_network avm_unix
    avm_esp32 avm_rp2 avm_stm32 avm_emscripten esp32devmode)
set(SUPPORTED_API_GATE_LIBS ${SUPPORTED_API_CONTRACT_LIBS} jit)

foreach(lib_name IN LISTS SUPPORTED_API_GATE_LIBS)
    supported_api_fragment(${lib_name})
    set(SUPPORTED_API_GATE_FRAGMENTS ${SUPPORTED_API_GATE_FRAGMENTS} ${SUPPORTED_API_${lib_name}_FUNCS})
endforeach()
foreach(lib_name IN LISTS SUPPORTED_API_CONTRACT_LIBS)
    set(SUPPORTED_API_CONTRACT_FRAGMENTS ${SUPPORTED_API_CONTRACT_FRAGMENTS} ${SUPPORTED_API_${lib_name}_FUNCS})
endforeach()

# The registrations are extracted on every run, from the source tree, so that
# adding a platform source cannot leave a stale list behind.
add_custom_target(check-native-stubs
    COMMAND ${CMAKE_COMMAND} -E make_directory ${SUPPORTED_API_DIR}
    COMMAND
        escript ${SUPPORTED_API_TOOLS}/extract_gperf_registrations.erl
            -o ${SUPPORTED_API_DIR}/registrations.txt
            src/libAtomVM/bifs.gperf src/libAtomVM/nifs.gperf
    COMMAND
        escript ${SUPPORTED_API_TOOLS}/check_native_stubs.erl
            --registrations ${SUPPORTED_API_DIR}/registrations.txt
            ${SUPPORTED_API_GATE_FRAGMENTS}
    DEPENDS ${SUPPORTED_API_GATE_FRAGMENTS}
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "Checking that every natively registered function has an Erlang export"
    VERBATIM
)
add_dependencies(check-native-stubs ${SUPPORTED_API_GATE_LIBS})

add_custom_command(
    OUTPUT ${SUPPORTED_API_DIR}/funcs.txt
    DEPENDS ${SUPPORTED_API_CONTRACT_FRAGMENTS}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${SUPPORTED_API_DIR}
    COMMAND
        cat ${SUPPORTED_API_CONTRACT_FRAGMENTS} | LC_ALL=C sort -u > ${SUPPORTED_API_DIR}/funcs.txt
    COMMENT "Generating supported_api/funcs.txt"
)

add_custom_target(supported_api
    DEPENDS ${SUPPORTED_API_DIR}/funcs.txt
    COMMENT "Generating the supported API files"
)
add_dependencies(supported_api ${SUPPORTED_API_CONTRACT_LIBS})

if (NOT Elixir_FOUND OR NOT Gleam_FOUND)
    message(WARNING "Elixir and/or Gleam were not found -- supported_api/funcs.txt would be incomplete")
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
