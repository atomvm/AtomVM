#
# This file is part of AtomVM.
#
# Copyright 2026 Peter M. <petermm@gmail.com>
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

cmake_minimum_required(VERSION 3.13)

set(PROJECT_EXECUTABLE ${PROJECT_NAME}-${RTEMS_BSP_NAME}.exe)

if(NOT AVM_PACK)
    set(_default_avm "${CMAKE_BINARY_DIR}/rtems_boot.avm")
    if(EXISTS "${_default_avm}")
        set(AVM_PACK "${_default_avm}")
        message(STATUS "Using default AVM pack ${_default_avm}")
    endif()
endif()

set(EMBEDDED_AVM_C "${CMAKE_CURRENT_BINARY_DIR}/embedded_avm.c")
set(_embed_avm_depends ${CMAKE_CURRENT_SOURCE_DIR}/../cmake/embed_avm.cmake)
if(AVM_PACK)
    list(APPEND _embed_avm_depends ${AVM_PACK})
endif()
add_custom_command(
    OUTPUT ${EMBEDDED_AVM_C}
    COMMAND ${CMAKE_COMMAND}
        -DAVM_IN=${AVM_PACK}
        -DAVM_OUT=${EMBEDDED_AVM_C}
        -P ${CMAKE_CURRENT_SOURCE_DIR}/../cmake/embed_avm.cmake
    DEPENDS ${_embed_avm_depends}
    COMMENT "Embedding AVM pack into ${EMBEDDED_AVM_C}"
    VERBATIM
)

add_executable(${PROJECT_EXECUTABLE}
    main.c
    rtems_config.c
    ${EMBEDDED_AVM_C}
)

target_compile_features(${PROJECT_EXECUTABLE} PUBLIC c_std_11)
if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(${PROJECT_EXECUTABLE} PUBLIC -Wall -pedantic -Wextra -ggdb)
endif()

include(SystemArchitecture)
avm_get_system_architecture_string(AVM_SYSTEM_ARCHITECTURE_STRING PLATFORM_VENDOR rtems PLATFORM_OS rtems)
add_subdirectory(../../../libAtomVM libAtomVM)
target_link_libraries(${PROJECT_EXECUTABLE} PUBLIC libAtomVM)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

add_subdirectory(lib)
target_link_libraries(${PROJECT_EXECUTABLE} PRIVATE libAtomVM${PLATFORM_LIB_SUFFIX})

if(RTEMS_SIZE)
    add_custom_command(
        TARGET ${PROJECT_EXECUTABLE}
        POST_BUILD
        COMMAND ${RTEMS_SIZE} ${PROJECT_EXECUTABLE}
        WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
    )
endif()

message("------------Output Locations------------")
message(STATUS "EXE: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${PROJECT_EXECUTABLE}")
if(AVM_PACK)
    message(STATUS "AVM: ${AVM_PACK}")
else()
    message(STATUS "AVM: (none embedded; executable will refuse to boot)")
endif()
