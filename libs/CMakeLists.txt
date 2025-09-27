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
add_subdirectory(esp32boot)
add_subdirectory(esp32devmode/src)
# JIT compiler doesn't compile with OTP < 23
if(Erlang_VERSION VERSION_GREATER_EQUAL "23")
    add_subdirectory(jit/src)
endif()


set(ATOMVM_LIBS eavmlib estdlib alisp)

find_package(Elixir)
find_package(Gleam)

if (Elixir_FOUND)
    add_subdirectory(exavmlib/lib)
    list(APPEND ATOMVM_LIBS exavmlib)
else()
    message(WARNING "Unable to find elixirc -- skipping Elixir libs")
endif()

if (Gleam_FOUND)
    add_subdirectory(gleam_avm)
    list(APPEND ATOMVM_LIBS gleam_avm)
else()
    message(WARNING "Unable to find gleam -- skipping Gleam libs")
endif()

pack_lib(atomvmlib ${ATOMVM_LIBS})

if (Dialyzer_FOUND)
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/estdlib_beams.txt
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/estdlib/src/estdlib.avm PackBEAM
        COMMAND
            ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -l ${CMAKE_CURRENT_BINARY_DIR}/estdlib/src/estdlib.avm | sed -e 's|^|${CMAKE_CURRENT_BINARY_DIR}/estdlib/src/beams/|g' > ${CMAKE_CURRENT_BINARY_DIR}/estdlib_beams.txt
    )
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/eavmlib_beams.txt
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/eavmlib/src/eavmlib.avm PackBEAM
        COMMAND
            ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -l ${CMAKE_CURRENT_BINARY_DIR}/eavmlib/src/eavmlib.avm | sed -e 's|^|${CMAKE_CURRENT_BINARY_DIR}/eavmlib/src/beams/|g' > ${CMAKE_CURRENT_BINARY_DIR}/eavmlib_beams.txt
    )
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/alisp_beams.txt
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/alisp/src/alisp.avm PackBEAM
        COMMAND
            ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -l ${CMAKE_CURRENT_BINARY_DIR}/alisp/src/alisp.avm | sed -e 's|^|${CMAKE_CURRENT_BINARY_DIR}/alisp/src/beams/|g' > ${CMAKE_CURRENT_BINARY_DIR}/alisp_beams.txt
    )
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/jit_beams.txt
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/jit/src/jit.avm PackBEAM
        COMMAND
            ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -l ${CMAKE_CURRENT_BINARY_DIR}/jit/src/jit.avm | sed -e 's|^|${CMAKE_CURRENT_BINARY_DIR}/jit/src/beams/|g' | grep -v jit_precompile > ${CMAKE_CURRENT_BINARY_DIR}/jit_beams.txt
    )
    set(dialyzer_lists
            ${CMAKE_CURRENT_BINARY_DIR}/estdlib_beams.txt
            ${CMAKE_CURRENT_BINARY_DIR}/eavmlib_beams.txt
            ${CMAKE_CURRENT_BINARY_DIR}/alisp_beams.txt
    )
    if(Erlang_VERSION VERSION_GREATER_EQUAL "23")
        set(dialyzer_lists ${dialyzer_lists} ${CMAKE_CURRENT_BINARY_DIR}/jit_beams.txt)
    endif()
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/atomvmlib.plt
        DEPENDS ${dialyzer_lists}
        COMMAND cat ${dialyzer_lists}
            | xargs dialyzer --build_plt --output_plt ${CMAKE_CURRENT_BINARY_DIR}/atomvmlib.plt
    )
    add_custom_target(atomvmlib_plt
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/atomvmlib.plt
    )
else()
    message("Dialyzer was not found -- skipping PLT build")
endif()

install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/atomvmlib.avm
    DESTINATION lib/atomvm
)
if(NOT AVM_DISABLE_JIT)
install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/atomvmlib-${AVM_JIT_TARGET_ARCH}.avm
    DESTINATION lib/atomvm/
)
endif()
