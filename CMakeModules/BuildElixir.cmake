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

macro(mix_packbeam lib_name)
    find_package(Elixir REQUIRED)

    if(AVM_RELEASE)
        set(INCLUDE_LINES "")
    else()
        set(INCLUDE_LINES "-i")
    endif()
    
    add_custom_target(
        ${lib_name}_deps ALL
        DEPENDS mix-prepare-${lib_name}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND mix deps.get
        VERBATIM
    )

    add_custom_target(
        ${lib_name}_packbeam ALL
        DEPENDS ${lib_name}_deps
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND mix atomvm.packbeam ${INCLUDE_LINES}
        COMMENT "Creating ${lib_name} packbeam ..."
        VERBATIM
    )

    add_custom_target(
        ${lib_name} ALL
        DEPENDS ${lib_name}_packbeam
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND touch -c ${lib_name}.avm
        VERBATIM
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
        set(INCLUDE_LINES "")
    else()
        set(INCLUDE_LINES "-i")
    endif()

    foreach(archive_name ${ARGN})
        if(${archive_name} STREQUAL "exavmlib")
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/${archive_name}.avm)
        else()
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/${archive_name}.avm)
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
