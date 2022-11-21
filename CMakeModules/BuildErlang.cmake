#
# This file is part of AtomVM.
#
# Copyright 2018-2020 Fred Dushin <fred@dushin.net>
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

    foreach(module_name ${ARGN})
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/beams/${module_name}.beam
            COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/beams && erlc -o ${CMAKE_CURRENT_BINARY_DIR}/beams -I ${CMAKE_SOURCE_DIR}/libs/include ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
            COMMENT "Compiling ${module_name}.erl"
            VERBATIM
        )
        set(BEAMS ${BEAMS} ${CMAKE_CURRENT_BINARY_DIR}/beams/${module_name}.beam)
    endforeach()

    add_custom_target(
        ${avm_name}_beams ALL
        DEPENDS ${BEAMS}
    )

    add_custom_target(
        ${avm_name} ALL
        DEPENDS ${avm_name}_beams PackBEAM
        #DEPENDS ${BEAMS}
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -a ${avm_name}.avm ${BEAMS}
        COMMENT "Packing archive ${avm_name}.avm"
        VERBATIM
    )
    add_dependencies(${avm_name} ${avm_name}_beams PackBEAM)

endmacro()


macro(pack_lib avm_name)

    foreach(archive_name ${ARGN})
        if(NOT ${archive_name} STREQUAL "exavmlib")
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}.avm)
        else()
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/lib/${archive_name}.avm)
        endif()
        set(ARCHIVE_TARGETS ${ARCHIVE_TARGETS} ${archive_name})
    endforeach()

    add_custom_target(
        ${avm_name} ALL
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -a ${avm_name}.avm ${ARCHIVES}
        COMMENT "Packing runnable ${avm_name}.avm"
        VERBATIM
    )
    add_dependencies(${avm_name} ${ARCHIVE_TARGETS} PackBEAM)
    if(TARGET ${avm_name}_main)
        add_dependencies(${avm_name} ${avm_name}_main)
    endif()

endmacro()


macro(pack_runnable avm_name main)

    add_custom_command(
        OUTPUT ${main}.beam
        COMMAND erlc -I ${CMAKE_SOURCE_DIR}/libs/include ${CMAKE_CURRENT_SOURCE_DIR}/${main}.erl
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${main}.erl
        COMMENT "Compiling ${main}.erl"
        VERBATIM
    )

    add_custom_target(
        ${avm_name}_main
        DEPENDS ${main}.beam
    )

    foreach(archive_name ${ARGN})
        if(NOT ${archive_name} STREQUAL "exavmlib")
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}.avm)
        else()
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/lib/${archive_name}.avm)
        endif()
        set(ARCHIVE_TARGETS ${ARCHIVE_TARGETS} ${archive_name})
    endforeach()

    add_custom_target(
        ${avm_name} ALL
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM ${avm_name}.avm ${main}.beam ${ARCHIVES}
        COMMENT "Packing runnable ${avm_name}.avm"
        VERBATIM
    )
    add_dependencies(${avm_name} ${avm_name}_main ${ARCHIVE_TARGETS} PackBEAM)

endmacro()


macro(pack_test test_avm_name)

    set(ARCHIVES ${CMAKE_CURRENT_BINARY_DIR}/${test_avm_name}_lib.avm)
    set(ARCHIVE_TARGETS ${test_avm_name}_lib)
    foreach(archive_name ${ARGN})
        set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}.avm)
        set(ARCHIVE_TARGETS ${ARCHIVE_TARGETS} ${archive_name})
    endforeach()

    add_custom_target(
        ${test_avm_name} ALL
        COMMAND erlc -I ${CMAKE_SOURCE_DIR}/libs/include ${CMAKE_CURRENT_SOURCE_DIR}/tests.erl
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM ${CMAKE_CURRENT_BINARY_DIR}/${test_avm_name}.avm ${CMAKE_CURRENT_BINARY_DIR}/tests.beam ${ARCHIVES}
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/tests.erl
        COMMENT "Packing runnable ${test_avm_name}.avm"
        VERBATIM
    )
    add_dependencies(${test_avm_name} ${ARCHIVE_TARGETS} PackBEAM)

endmacro()
