#
# This file is part of AtomVM.
#
# Copyright 2018-2023 Fred Dushin <fred@dushin.net>
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

##
## Usage:
##      rebar3_packbeam(project_name [dep_1, ..., dep_n])
##          where each dep_i is a dependent project
##
macro(rebar3_packbeam project_name)
    set(rebar3_packbeam_${project_name}_deps "")
    set(rebar3_packbeam_${project_name}_ext "")
    foreach(archive ${ARGN})
        set(rebar3_packbeam_${project_name}_deps ${rebar3_packbeam_${project_name}_deps} ${archive})
        set(rebar3_packbeam_${project_name}_ext ${rebar3_packbeam_${project_name}_ext} "-e ${CMAKE_BINARY_DIR}/libs/${archive}/${archive}.avm")
    endforeach()

    foreach(dep ${rebar3_packbeam_${project_name}_deps})
        add_custom_target(
            ${project_name}_${dep}_checkouts ALL
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            COMMAND mkdir -p _checkouts && cd _checkouts && ln -f -s ${CMAKE_SOURCE_DIR}/libs/${dep}
            VERBATIM
        )
    endforeach()

    add_custom_target(
        ${project_name}_packbeam ALL
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMAND rebar3 atomvm packbeam ${rebar3_packbeam_${project_name}_ext}
        COMMAND_EXPAND_LISTS
        COMMENT "Creating ${project_name} packbeam ..."
        VERBATIM
    )

    add_custom_target(
        ${project_name} ALL
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ln -f -s _build/default/lib/${project_name}.avm
        DEPENDS ${project_name}_packbeam
        VERBATIM
    )

    foreach(dep ${rebar3_packbeam_${project_name}_deps})
        add_dependencies(${project_name}_packbeam ${dep}_packbeam)
        add_dependencies(${project_name}_packbeam ${dep})
        add_dependencies(${project_name}_packbeam ${project_name}_${dep}_checkouts)
    endforeach()

endmacro()

##
## TODO only used by exavm -- deprecate once we have a solution for mix
##
macro(pack_archive avm_name)

    foreach(module_name ${ARGN})
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/beams/${module_name}.beam
            COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/beams && erlc +debug_info -o ${CMAKE_CURRENT_BINARY_DIR}/beams -I ${CMAKE_SOURCE_DIR}/libs/estdlib/include -I ${CMAKE_SOURCE_DIR}/libs/eavmlib/include ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
            COMMENT "Compiling ${module_name}.erl"
            VERBATIM
        )
        set(pack_archive_${avm_name}_beams ${pack_archive_${avm_name}_beams} ${CMAKE_CURRENT_BINARY_DIR}/beams/${module_name}.beam)
    endforeach()

    add_custom_target(
        ${avm_name}_beams ALL
        DEPENDS ${pack_archive_${avm_name}_beams}
    )

    if(AVM_RELEASE)
        set(INCLUDE_LINES "")
    else()
        set(INCLUDE_LINES "-i")
    endif()

    add_custom_target(
        ${avm_name} ALL
        DEPENDS ${avm_name}_beams PackBEAM
        #DEPENDS ${pack_archive_${avm_name}_beams}
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -a ${INCLUDE_LINES} ${avm_name}.avm ${pack_archive_${avm_name}_beams}
        COMMENT "Packing archive ${avm_name}.avm"
        VERBATIM
    )
    add_dependencies(${avm_name} ${avm_name}_beams PackBEAM)

endmacro()

##
## TODO only used in examples -- to deprecate
##
macro(pack_runnable avm_name main)
    add_custom_command(
        OUTPUT ${main}.beam
        COMMAND erlc +debug_info -I ${CMAKE_SOURCE_DIR}/libs/estdlib/include -I ${CMAKE_SOURCE_DIR}/libs/eavmlib/include ${CMAKE_CURRENT_SOURCE_DIR}/${main}.erl
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
            set(pack_runnable_${avm_name}_archives ${pack_runnable_${avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/${archive_name}/${archive_name}.avm)
            if(NOT ${archive_name} MATCHES "^eavmlib|estdlib$")
                set(${avm_name}_dialyzer_beams_opt ${${avm_name}_dialyzer_beams_opt} "-r" ${CMAKE_BINARY_DIR}/libs/${archive_name}/_build/default/lib/${archive_name}/ebin)
            endif()
        else()
            set(pack_runnable_${avm_name}_archives ${pack_runnable_${avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/${archive_name}/lib/${archive_name}.avm)
        endif()
        set(pack_runnable_${avm_name}_archive_targets ${pack_runnable_${avm_name}_archive_targets} ${archive_name})
    endforeach()

    if (Dialyzer_FOUND)
        add_custom_target(
            ${avm_name}_dialyzer
            DEPENDS ${avm_name}_main
            COMMAND dialyzer --plt ${CMAKE_BINARY_DIR}/libs/atomvmlib.plt -c ${main}.beam ${${avm_name}_dialyzer_beams_opt}
        )
        add_dependencies(${avm_name}_dialyzer atomvmlib_plt ${pack_runnable_${avm_name}_archive_targets})
        add_dependencies(dialyzer ${avm_name}_dialyzer)
    endif()

    if(AVM_RELEASE)
        set(INCLUDE_LINES "")
    else()
        set(INCLUDE_LINES "-i")
    endif()

    add_custom_target(
        ${avm_name} ALL
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM ${INCLUDE_LINES} ${avm_name}.avm ${main}.beam ${pack_runnable_${avm_name}_archives}
        COMMENT "Packing runnable ${avm_name}.avm"
        VERBATIM
    )
    add_dependencies(${avm_name} ${avm_name}_main ${pack_runnable_${avm_name}_archive_targets} PackBEAM)
endmacro()

##
## TODO only used in examples -- to deprecate
##
macro(pack_uf2 avm_name main)

    add_custom_command(
        OUTPUT ${main}.beam
        COMMAND erlc +debug_info -I ${CMAKE_SOURCE_DIR}/libs/include ${CMAKE_CURRENT_SOURCE_DIR}/${main}.erl
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
            set(pack_uf2_${avm_name}_archives ${pack_uf2_${avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/${archive_name}/${archive_name}.avm)
        else()
            set(pack_uf2_${avm_name}_archives ${pack_uf2_${avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/${archive_name}/lib/${archive_name}.avm)
        endif()
        set(pack_uf2_${avm_name}_archive_targets ${pack_uf2_${avm_name}_archive_targets} ${archive_name})
    endforeach()

    add_custom_target(
        ${avm_name}.avm ALL
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM ${avm_name}.avm ${main}.beam ${pack_uf2_${avm_name}_archives}
        COMMENT "Packing runnable ${avm_name}.avm"
        VERBATIM
    )
    add_dependencies(${avm_name}.avm ${avm_name}_main ${pack_uf2_${avm_name}_archive_targets} PackBEAM)

    add_custom_target(
        ${avm_name}.uf2 ALL
        COMMAND ${CMAKE_BINARY_DIR}/tools/uf2tool/uf2tool create -o ${avm_name}.uf2 -s 0x10180000 ${avm_name}.avm
        COMMENT "Creating UF2 file ${avm_name}.uf2"
        VERBATIM
    )
    add_dependencies(${avm_name}.uf2 ${avm_name}.avm uf2tool)

endmacro()
