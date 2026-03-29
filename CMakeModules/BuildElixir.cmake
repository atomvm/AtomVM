#
# This file is part of AtomVM.
#
# Copyright 2019-2020 Riccardo Binetti <rbino@gmx.com>
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

# Compile a single .ex source that defines multiple modules.
# All module names must be listed, and OUTPUT names the variable
# that receives the beam paths (for passing to pack_archive).
#
# Usage:
#     compile_multi(json.ex
#         JSON
#         JSON.Encoder
#         JSON.Encoder.Atom
#         OUTPUT JSON_BEAMS
#     )
#     pack_archive(mylib ${MODULES} EXTRA_BEAMS ${JSON_BEAMS})
#
macro(compile_multi source_file)
    find_package(Elixir REQUIRED)
    set(_cm_modules "")
    set(_cm_outputs "")
    set(_cm_outvar "")
    set(_cm_in_output FALSE)
    foreach(_arg ${ARGN})
        if(_arg STREQUAL "OUTPUT")
            set(_cm_in_output TRUE)
        elseif(_cm_in_output)
            set(_cm_outvar "${_arg}")
        else()
            list(APPEND _cm_modules "${_arg}")
            list(APPEND _cm_outputs
                "${CMAKE_CURRENT_BINARY_DIR}/beams/Elixir.${_arg}.beam"
            )
        endif()
    endforeach()
    add_custom_command(
        OUTPUT ${_cm_outputs}
        COMMAND mkdir -p
            ${CMAKE_CURRENT_BINARY_DIR}/beams
            ${CMAKE_CURRENT_BINARY_DIR}/beams.tmp.multi.${_cm_outvar}
        COMMAND elixirc --no-docs --no-debug-info
            --ignore-module-conflict
            -o ${CMAKE_CURRENT_BINARY_DIR}/beams.tmp.multi.${_cm_outvar}/
            ${CMAKE_CURRENT_SOURCE_DIR}/${source_file}
        COMMAND sh -c
            "mv ${CMAKE_CURRENT_BINARY_DIR}/beams.tmp.multi.${_cm_outvar}/Elixir.*.beam ${CMAKE_CURRENT_BINARY_DIR}/beams/"
        COMMAND rmdir
            ${CMAKE_CURRENT_BINARY_DIR}/beams.tmp.multi.${_cm_outvar}
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${source_file}
        COMMENT "Compiling ${source_file}"
        VERBATIM
    )
    set(${_cm_outvar} ${_cm_outputs})
endmacro()

macro(pack_archive avm_name)
    find_package(Elixir REQUIRED)

    # Parse EXTRA_BEAMS keyword from arguments
    set(_pa_modules "")
    set(_pa_extra "")
    set(_pa_in_extra FALSE)
    foreach(_arg ${ARGN})
        if(_arg STREQUAL "EXTRA_BEAMS")
            set(_pa_in_extra TRUE)
        elseif(_pa_in_extra)
            list(APPEND _pa_extra "${_arg}")
        else()
            list(APPEND _pa_modules "${_arg}")
        endif()
    endforeach()

    foreach(module_name ${_pa_modules})
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/beams/Elixir.${module_name}.beam
            COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/beams ${CMAKE_CURRENT_BINARY_DIR}/beams.tmp.${module_name}
            COMMAND elixirc --no-docs --no-debug-info --ignore-module-conflict -o ${CMAKE_CURRENT_BINARY_DIR}/beams.tmp.${module_name}/ ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.ex
            COMMAND sh -c "mv ${CMAKE_CURRENT_BINARY_DIR}/beams.tmp.${module_name}/Elixir.*.beam ${CMAKE_CURRENT_BINARY_DIR}/beams/"
            COMMAND rmdir ${CMAKE_CURRENT_BINARY_DIR}/beams.tmp.${module_name}
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.ex
            COMMENT "Compiling ${module_name}.ex"
            VERBATIM
        )
        set(BEAMS ${BEAMS} ${CMAKE_CURRENT_BINARY_DIR}/beams/Elixir.${module_name}.beam)
    endforeach()

    set(BEAMS ${BEAMS} ${_pa_extra})

    add_custom_target(
        ${avm_name}_beams ALL
        DEPENDS ${BEAMS}
    )

    if(AVM_RELEASE)
        set(INCLUDE_LINES "--remove_lines")
    else()
        set(INCLUDE_LINES "")
    endif()

    add_custom_command(
        OUTPUT ${avm_name}.avm
        DEPENDS ${avm_name}_beams PackBEAM ${BEAMS}
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/packbeam create ${INCLUDE_LINES} ${avm_name}.avm ${BEAMS}
        COMMENT "Packing archive ${avm_name}.avm"
        VERBATIM
    )

    add_custom_target(
        ${avm_name} ALL
        DEPENDS ${avm_name}.avm
    )
endmacro()

macro(pack_runnable avm_name main)
    find_package(Elixir REQUIRED)

    add_custom_command(
        OUTPUT Elixir.${main}.beam
        COMMAND elixirc ${CMAKE_CURRENT_SOURCE_DIR}/${main}.ex
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${main}.ex
        COMMENT "Compiling ${main}.ex"
        VERBATIM
    )

    add_custom_target(
        ${avm_name}_main
        DEPENDS Elixir.${main}.beam
    )

    if(AVM_RELEASE)
        set(INCLUDE_LINES "--remove_lines")
    else()
        set(INCLUDE_LINES "")
    endif()
    set(PACKBEAM_PRUNE_ARGS "")
    if(AVM_PRUNE_RUNNABLES)
        set(PACKBEAM_PRUNE_ARGS "-p")
    endif()

    foreach(archive_name ${ARGN})
        if(${archive_name} STREQUAL "exavmlib")
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/lib/${archive_name}.avm)
        else()
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}.avm)
        endif()
        set(ARCHIVE_TARGETS ${ARCHIVE_TARGETS} ${archive_name})
    endforeach()

    add_custom_command(
        OUTPUT ${avm_name}.avm
        DEPENDS ${avm_name}_main ${ARCHIVE_TARGETS} PackBEAM Elixir.${main}.beam ${ARCHIVES}
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/packbeam create ${PACKBEAM_PRUNE_ARGS} -s Elixir.${main} ${INCLUDE_LINES} ${avm_name}.avm Elixir.${main}.beam ${ARCHIVES}
        COMMENT "Packing runnable ${avm_name}.avm"
        VERBATIM
    )

    add_custom_target(
        ${avm_name} ALL
        DEPENDS ${avm_name}.avm
    )

endmacro()


macro(pack_test avm_name main)
    find_package(Elixir REQUIRED)

    if(AVM_DISABLE_JIT)
        set(precompiled_suffix "")
    else()
        set(precompiled_suffix "-${AVM_JIT_TARGET_ARCH}")
        set(ARCHIVES_TARGETS jit)
    endif()

    # Compile the main module
    add_custom_command(
        OUTPUT Elixir.${main}.beam
        COMMAND elixirc ${CMAKE_CURRENT_SOURCE_DIR}/${main}.ex
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${main}.ex
        COMMENT "Compiling ${main}.ex"
        VERBATIM
    )

    add_custom_target(
        ${avm_name}_main
        DEPENDS Elixir.${main}.beam
    )

    # Compile test modules
    foreach(module_name ${ARGN})
        add_custom_command(
            OUTPUT Elixir.${module_name}.beam
            COMMAND elixirc ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.ex
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.ex
            COMMENT "Compiling ${module_name}.ex"
            VERBATIM
        )
        set(TEST_BEAMS ${TEST_BEAMS} Elixir.${module_name}.beam)
    endforeach()

    add_custom_target(
        ${avm_name}_tests
        DEPENDS ${TEST_BEAMS}
    )

    if(AVM_RELEASE)
        set(INCLUDE_LINES "--remove_lines")
    else()
        set(INCLUDE_LINES "")
    endif()

    # Set up standard libraries
    set(ARCHIVE_TARGETS ${ARCHIVES_TARGETS} estdlib eavmlib exavmlib etest)
    foreach(archive_name ${ARCHIVE_TARGETS})
        if(${archive_name} STREQUAL "exavmlib")
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/lib/${archive_name}.avm)
        elseif(${archive_name} MATCHES "^estdlib$")
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}${precompiled_suffix}.avm)
        elseif(${archive_name} MATCHES "^jit$")
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/jit/src/jit${precompiled_suffix}.avm)
        else()
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}.avm)
        endif()
    endforeach()

    add_custom_command(
        OUTPUT ${avm_name}.avm
        DEPENDS ${avm_name}_main ${avm_name}_tests ${ARCHIVE_TARGETS} PackBEAM Elixir.${main}.beam ${TEST_BEAMS} ${ARCHIVES}
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/packbeam create ${INCLUDE_LINES} ${avm_name}.avm Elixir.${main}.beam ${TEST_BEAMS} ${ARCHIVES}
        COMMENT "Packing test ${avm_name}.avm"
        VERBATIM
    )

    add_custom_target(
        ${avm_name} ALL
        DEPENDS ${avm_name}.avm
    )
endmacro()
