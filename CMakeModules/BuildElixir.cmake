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

macro(pack_archive avm_name)
    find_package(Elixir REQUIRED)

    foreach(module_name ${ARGN})
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/beams/Elixir.${module_name}.beam
            COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/beams && elixirc --no-docs --no-debug-info --ignore-module-conflict -o ${CMAKE_CURRENT_BINARY_DIR}/beams ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.ex
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.ex
            COMMENT "Compiling ${module_name}.ex"
            VERBATIM
        )
        set(BEAMS ${BEAMS} ${CMAKE_CURRENT_BINARY_DIR}/beams/Elixir.${module_name}.beam)
    endforeach()

    add_custom_target(
        ${avm_name}_beams ALL
        DEPENDS ${BEAMS}
    )

    if(AVM_RELEASE)
        set(INCLUDE_LINES "")
    else()
        set(INCLUDE_LINES "-i")
    endif()

    add_custom_target(
        ${avm_name} ALL
        DEPENDS ${avm_name}_beams PackBEAM
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -a ${INCLUDE_LINES} ${avm_name}.avm ${BEAMS}
        COMMENT "Packing archive ${avm_name}.avm"
        VERBATIM
    )
    add_dependencies(${avm_name} ${avm_name}_beams PackBEAM)

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
        set(INCLUDE_LINES "")
    else()
        set(INCLUDE_LINES "-i")
    endif()

    foreach(archive_name ${ARGN})
        if(${archive_name} STREQUAL "exavmlib")
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/lib/${archive_name}.avm)
        else()
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}.avm)
        endif()
        set(ARCHIVE_TARGETS ${ARCHIVE_TARGETS} ${archive_name})
    endforeach()

    add_custom_target(
        ${avm_name} ALL
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM ${INCLUDE_LINES} ${avm_name}.avm Elixir.${main}.beam ${ARCHIVES}
        COMMENT "Packing runnable ${avm_name}.avm"
        VERBATIM
    )
    add_dependencies(${avm_name} ${avm_name}_main ${ARCHIVE_TARGETS} PackBEAM)

endmacro()
