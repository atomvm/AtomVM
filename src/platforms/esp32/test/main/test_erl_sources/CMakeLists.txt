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

add_library(esp32_test_modules)

# CONFIG_* is available here because idf_component_register was already called
# by the parent main/CMakeLists.txt. Replicate the JIT detection logic from
# components/libatomvm/CMakeLists.txt since that directory's variables do not
# propagate into this sibling subdirectory scope.
if(CONFIG_JIT_ENABLED)
    if (${IDF_TARGET} MATCHES "esp32c2|esp32c3|esp32c5|esp32c6|esp32c61|esp32h2|esp32p4")
        set(AVM_DISABLE_JIT OFF)
        set(AVM_JIT_TARGET_ARCH riscv32)
    elseif(${IDF_TARGET} MATCHES "esp32|esp32s2|esp32s3")
        set(AVM_DISABLE_JIT OFF)
        set(AVM_JIT_TARGET_ARCH xtensa)
    else()
        set(AVM_DISABLE_JIT ON)
    endif()
else()
    set(AVM_DISABLE_JIT ON)
endif()

include(ExternalProject)
if(NOT AVM_DISABLE_JIT)
    # AVM_ENABLE_PRECOMPILED is needed for esp32boot-${arch}.avm (AOT-compiled
    # estdlib/eavmlib/avm_esp32) which the JIT-precompiled tests rely on for
    # estdlib NIFs in AVM_NO_EMU mode.
    set(host_atomvm_cmake_args "-DAVM_ENABLE_PRECOMPILED=ON")
    set(host_atomvm_extra_targets "--target=jit" "--target=esp32boot")
else()
    set(host_atomvm_cmake_args "")
    set(host_atomvm_extra_targets "")
endif()
ExternalProject_Add(HostAtomVM
    SOURCE_DIR ../../../../../../../../
    CMAKE_ARGS ${host_atomvm_cmake_args}
    INSTALL_COMMAND cmake -E echo "Skipping install step."
    BUILD_COMMAND cmake --build . --target=atomvmlib-esp32 ${host_atomvm_extra_targets} --target=PackBEAM
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

function(compile_erlang module_name)
    add_custom_command(
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.beam"
        COMMAND erlc ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
        DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl"
        COMMENT "Compiling ${module_name}.erl"
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
    jit_precompile(${module_name})

    set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.beam")
endfunction()

# Compile an Erlang module WITHOUT JIT precompilation.
# Used to include plain BEAM modules that will be compiled at runtime by jit_stream_flash.
function(compile_erlang_no_jit module_name)
    add_custom_command(
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.beam"
        COMMAND erlc ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
        DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl"
        COMMENT "Compiling ${module_name}.erl (no JIT precompilation)"
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
    set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.beam")
endfunction()

compile_erlang(test_jit_compile)
compile_erlang(test_jit_simple)
compile_erlang(test_esp_partition)
compile_erlang(test_esp_timer_get_time)
compile_erlang(test_file)
compile_erlang(test_wifi_example)
compile_erlang(test_wifi_managed)
compile_erlang(test_list_to_atom)
compile_erlang(test_list_to_binary)
compile_erlang(test_md5)
compile_erlang(test_crypto)
compile_erlang(test_monotonic_time)
compile_erlang(test_mount)
compile_erlang(test_net)
compile_erlang(test_rtc_slow)
compile_erlang(test_select)
compile_erlang(test_socket)
compile_erlang(test_ssl)
compile_erlang(test_system_architecture)
compile_erlang(test_time_and_processes)
compile_erlang(test_twdt)
compile_erlang(test_tz)
compile_erlang(test_deep_sleep_hold)
compile_erlang(test_wifi_scan)

set(erlang_test_beams
    test_jit_compile.beam
    test_jit_simple.beam
    test_esp_partition.beam
    test_esp_timer_get_time.beam
    test_file.beam
    test_wifi_example.beam
    test_wifi_managed.beam
    test_list_to_atom.beam
    test_list_to_binary.beam
    test_md5.beam
    test_crypto.beam
    test_monotonic_time.beam
    test_mount.beam
    test_net.beam
    test_rtc_slow.beam
    test_select.beam
    test_socket.beam
    test_ssl.beam
    test_system_architecture.beam
    test_time_and_processes.beam
    test_twdt.beam
    test_tz.beam
    test_deep_sleep_hold.beam
    test_wifi_scan.beam
)

if(NOT AVM_DISABLE_JIT)
    set(erlang_test_beams_${AVM_JIT_TARGET_ARCH} ${erlang_test_beams})
    list(TRANSFORM erlang_test_beams_${AVM_JIT_TARGET_ARCH} PREPEND ${AVM_JIT_TARGET_ARCH}/)
    set(erlang_test_beams_to_package
        ${erlang_test_beams_${AVM_JIT_TARGET_ARCH}})
    set(erlang_test_beams_depends
        ${erlang_test_beams}
        ${erlang_test_beams_${AVM_JIT_TARGET_ARCH}})
    # In AVM_NO_EMU mode (JIT-only) tests need AOT-compiled estdlib/eavmlib/avm_esp32
    # to resolve calls like erlang:md5/1 (which is an estdlib wrapper around crypto:hash/2).
    # esp32boot-${arch}.avm packages exactly those, AOT-compiled for the target arch.
    set(atomvmlib_avm "HostAtomVM-prefix/src/HostAtomVM-build/libs/esp32boot/esp32boot-${AVM_JIT_TARGET_ARCH}.avm")
else()
    set(erlang_test_beams_to_package ${erlang_test_beams})
    set(erlang_test_beams_depends ${erlang_test_beams})
    set(atomvmlib_avm "HostAtomVM-prefix/src/HostAtomVM-build/libs/atomvmlib-esp32.avm")
endif()

add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/esp32_test_modules.avm"
    COMMAND HostAtomVM-prefix/src/HostAtomVM-build/tools/packbeam/packbeam create esp32_test_modules.avm
        ${atomvmlib_avm}
        ${erlang_test_beams_to_package}
    DEPENDS
        HostAtomVM
        ${erlang_test_beams_depends}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    VERBATIM
)

set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/esp32_test_modules.avm")

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/esp32_test_modules.avm.S
    COMMAND "${CMAKE_COMMAND}"
    -D "DATA_FILE=${CMAKE_CURRENT_BINARY_DIR}/esp32_test_modules.avm"
    -D "SOURCE_FILE=${CMAKE_CURRENT_BINARY_DIR}/esp32_test_modules.avm.S"
    -D "FILE_TYPE=BINARY"
    -P "${idf_path}/tools/cmake/scripts/data_file_embed_asm.cmake"
    MAIN_DEPENDENCY "${CMAKE_CURRENT_BINARY_DIR}/esp32_test_modules.avm"
    DEPENDS "${idf_path}/tools/cmake/scripts/data_file_embed_asm.cmake"
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    VERBATIM
)

set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/esp32_test_modules.avm.S")

target_sources(esp32_test_modules PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/esp32_test_modules.avm.S)
