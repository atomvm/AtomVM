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

macro(pack_gleam_archive avm_name)
    find_package(Gleam REQUIRED)

    foreach(module_name ${ARGN})
        string(REPLACE "/" "@" beam_name ${module_name})
        list(APPEND SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/${module_name}.gleam)
        list(APPEND BEAMS ${CMAKE_CURRENT_BINARY_DIR}/build/prod/erlang/gleam_avm/ebin/${beam_name}.beam)
    endforeach()

    if(AVM_RELEASE)
        set(INCLUDE_LINES "")
    else()
        set(INCLUDE_LINES "-i")
    endif()

    add_custom_command(
        OUTPUT ${avm_name}.avm
        DEPENDS ${SOURCES} PackBEAM
        COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/gleam.toml ${CMAKE_CURRENT_SOURCE_DIR}/manifest.toml ${CMAKE_CURRENT_BINARY_DIR}/
        COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_BINARY_DIR}/src
        COMMAND gleam export erlang-shipment
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -a ${INCLUDE_LINES} ${avm_name}.avm ${BEAMS}
        COMMENT "Packing gleam archive ${avm_name}.avm"
        VERBATIM
    )
    add_custom_target(${avm_name} ALL DEPENDS ${avm_name}.avm)

endmacro()

macro(pack_gleam_runnable avm_name main)
    find_package(Gleam REQUIRED)

    list(APPEND SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/${main}.gleam)
    list(APPEND BEAMS ${CMAKE_CURRENT_BINARY_DIR}/build/prod/erlang/${main}/ebin/${main}.beam)
    list(APPEND BEAMS ${CMAKE_CURRENT_BINARY_DIR}/build/prod/erlang/*/ebin/*.beam)

    if(AVM_RELEASE)
        set(INCLUDE_LINES "")
    else()
        set(INCLUDE_LINES "-i")
    endif()

    foreach(archive_name ${ARGN})
        if(${archive_name} STREQUAL "gleam_avm")
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/${archive_name}.avm)
        else()
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}.avm)
        endif()
        set(ARCHIVE_TARGETS ${ARCHIVE_TARGETS} ${archive_name})
    endforeach()

    add_custom_command(
        OUTPUT ${avm_name}.avm
        DEPENDS ${SOURCES} PackBEAM
        COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/gleam.toml ${CMAKE_CURRENT_SOURCE_DIR}/manifest.toml ${CMAKE_CURRENT_BINARY_DIR}/
        COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_BINARY_DIR}/src
        COMMAND gleam export erlang-shipment
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM ${INCLUDE_LINES} ${avm_name}.avm ${BEAMS} ${ARCHIVES}
        COMMENT "Packing gleam runnable ${avm_name}.avm"
    )

    add_custom_target(${avm_name} ALL DEPENDS ${avm_name}.avm ${ARCHIVE_TARGETS})

endmacro()
