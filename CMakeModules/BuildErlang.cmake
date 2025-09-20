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

    set(multiValueArgs ERLC_FLAGS MODULES)
    cmake_parse_arguments(PACK_ARCHIVE "" "" "${multiValueArgs}" ${ARGN})
    list(JOIN PACK_ARCHIVE_ERLC_FLAGS " " PACK_ARCHIVE_ERLC_FLAGS)
    foreach(module_name IN LISTS ${PACK_ARCHIVE_MODULES} PACK_ARCHIVE_MODULES PACK_ARCHIVE_UNPARSED_ARGUMENTS)
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/beams/${module_name}.beam
            COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/beams
                && erlc +debug_info ${PACK_ARCHIVE_ERLC_FLAGS}
                    -o ${CMAKE_CURRENT_BINARY_DIR}/beams
                    -I ${CMAKE_SOURCE_DIR}/libs/include
                    -I ${CMAKE_SOURCE_DIR}/libs
                    -I ${CMAKE_CURRENT_SOURCE_DIR}/../include
                    ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
            COMMENT "Compiling ${module_name}.erl"
            VERBATIM
        )
        set(pack_archive_${avm_name}_beams ${pack_archive_${avm_name}_beams} ${CMAKE_CURRENT_BINARY_DIR}/beams/${module_name}.beam)
    endforeach()

    if(AVM_RELEASE)
        set(INCLUDE_LINES "")
    else()
        set(INCLUDE_LINES "-i")
    endif()

    add_custom_command(
        OUTPUT ${avm_name}.avm
        DEPENDS ${pack_archive_${avm_name}_beams} PackBEAM
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -a ${INCLUDE_LINES} ${avm_name}.avm ${pack_archive_${avm_name}_beams}
        COMMENT "Packing archive ${avm_name}.avm"
        VERBATIM
    )
    # Create emu target for source beams only
    add_custom_target(
        ${avm_name}_emu
        DEPENDS ${avm_name}.avm
    )
    # Create main target (will be updated by pack_precompiled_archive if needed)
    add_custom_target(
        ${avm_name} ALL
        DEPENDS ${avm_name}_emu
    )
endmacro()

macro(pack_precompiled_archive avm_name)
    pack_archive(${avm_name} ${ARGN})

    set(multiValueArgs ERLC_FLAGS MODULES)
    cmake_parse_arguments(PACK_ARCHIVE "" "" "${multiValueArgs}" ${ARGN})

    if(NOT AVM_DISABLE_JIT OR AVM_ENABLE_PRECOMPILED)
        if(${avm_name} STREQUAL jit)
            set(jit_deps "")
        else()
            set(jit_deps "jit")
        endif()
        foreach(jit_target_arch_variant ${AVM_PRECOMPILED_TARGETS})
            set(pack_precompile_archive_${avm_name}_beams "")
            # Extract base architecture for module dependencies
            string(REGEX REPLACE "\\+.*$" "" jit_target_arch "${jit_target_arch_variant}")
            set(jit_compiler_modules
                ${CMAKE_BINARY_DIR}/libs/jit/src/beams/jit.beam
                ${CMAKE_BINARY_DIR}/libs/jit/src/beams/jit_precompile.beam
                ${CMAKE_BINARY_DIR}/libs/jit/src/beams/jit_stream_binary.beam
                ${CMAKE_BINARY_DIR}/libs/jit/src/beams/jit_${jit_target_arch}.beam
                ${CMAKE_BINARY_DIR}/libs/jit/src/beams/jit_${jit_target_arch}_asm.beam
            )

            foreach(module_name IN LISTS ${PACK_ARCHIVE_MODULES} PACK_ARCHIVE_MODULES PACK_ARCHIVE_UNPARSED_ARGUMENTS)
                add_custom_command(
                    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/beams/${jit_target_arch_variant}/${module_name}.beam
                    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/beams/${jit_target_arch_variant}/
                        && erl -pa ${CMAKE_BINARY_DIR}/libs/jit/src/beams/ -noshell -s jit_precompile -s init stop -- ${jit_target_arch_variant} ${CMAKE_CURRENT_BINARY_DIR}/beams/${jit_target_arch_variant}/ ${CMAKE_CURRENT_BINARY_DIR}/beams/${module_name}.beam
                    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/beams/${module_name}.beam ${jit_compiler_modules} ${jit_deps}
                    COMMENT "Compiling ${module_name}.beam to ${jit_target_arch_variant}"
                    VERBATIM
                )
                set(pack_precompile_archive_${avm_name}_beams ${pack_precompile_archive_${avm_name}_beams} ${CMAKE_CURRENT_BINARY_DIR}/beams/${jit_target_arch_variant}/${module_name}.beam)
            endforeach()

            if(AVM_RELEASE)
                set(INCLUDE_LINES "")
            else()
                set(INCLUDE_LINES "-i")
            endif()

            add_custom_command(
                OUTPUT ${avm_name}-${jit_target_arch_variant}.avm
                DEPENDS ${pack_precompile_archive_${avm_name}_beams} PackBEAM
                COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -a ${INCLUDE_LINES} ${avm_name}-${jit_target_arch_variant}.avm ${pack_precompile_archive_${avm_name}_beams}
                COMMENT "Packing archive ${avm_name}-${jit_target_arch_variant}.avm"
                VERBATIM
            )
            add_custom_target(
                ${avm_name}_${jit_target_arch_variant} ALL
                DEPENDS ${avm_name}-${jit_target_arch_variant}.avm
            )
            # Ensure source beams are built before precompilation
            add_dependencies(${avm_name}_${jit_target_arch_variant} ${avm_name}_emu)
            # Make main target depend on precompiled targets
            add_dependencies(${avm_name} ${avm_name}_${jit_target_arch_variant})
        endforeach()
    endif()
endmacro()

macro(pack_lib avm_name)
    set(pack_lib_${avm_name}_archive_targets "")
    if(NOT AVM_DISABLE_JIT)
        set(pack_lib_${avm_name}_archive_targets jit)
    endif()

    foreach(archive_name ${ARGN})
        if(${archive_name} STREQUAL "exavmlib")
            set(pack_lib_${avm_name}_archives ${pack_lib_${avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/${archive_name}/lib/${archive_name}.avm)
        elseif(${archive_name} STREQUAL "estdlib")
            set(pack_lib_${avm_name}_emu_archives ${pack_lib_${avm_name}_emu_archives} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}.avm)
        elseif(${archive_name} STREQUAL "gleam_avm")
            set(pack_lib_${avm_name}_archives ${pack_lib_${avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/${archive_name}/${archive_name}.avm)
        else()
            set(pack_lib_${avm_name}_archives ${pack_lib_${avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}.avm)
        endif()
        set(pack_lib_${avm_name}_archive_targets ${pack_lib_${avm_name}_archive_targets} ${archive_name})
    endforeach()

    if(AVM_RELEASE)
        set(INCLUDE_LINES "")
    else()
        set(INCLUDE_LINES "-i")
    endif()

    add_custom_command(
        OUTPUT ${avm_name}.avm
        DEPENDS ${pack_lib_${avm_name}_archive_targets} PackBEAM
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -a ${INCLUDE_LINES} ${avm_name}.avm ${pack_lib_${avm_name}_emu_archives} ${pack_lib_${avm_name}_archives}
        COMMENT "Packing lib ${avm_name}.avm"
        VERBATIM
    )
    set(target_deps ${avm_name}.avm)

    if(NOT AVM_DISABLE_JIT OR AVM_ENABLE_PRECOMPILED)
        foreach(jit_target_arch_variant ${AVM_PRECOMPILED_TARGETS})
            # Build JIT archives list for this specific target architecture
            set(pack_lib_${avm_name}_jit_archives_${jit_target_arch_variant} ${CMAKE_BINARY_DIR}/libs/jit/src/jit-${jit_target_arch_variant}.avm)
            foreach(archive_name ${ARGN})
                if(${archive_name} STREQUAL "estdlib")
                    set(pack_lib_${avm_name}_jit_archives_${jit_target_arch_variant} ${pack_lib_${avm_name}_jit_archives_${jit_target_arch_variant}} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}-${jit_target_arch_variant}.avm)
                endif()
            endforeach()

            add_custom_command(
                OUTPUT ${avm_name}-${jit_target_arch_variant}.avm
                DEPENDS ${pack_lib_${avm_name}_archive_targets} PackBEAM
                COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -a ${INCLUDE_LINES} ${avm_name}-${jit_target_arch_variant}.avm ${pack_lib_${avm_name}_jit_archives_${jit_target_arch_variant}} ${pack_lib_${avm_name}_archives}
                COMMENT "Packing lib ${avm_name}-${jit_target_arch_variant}.avm"
                VERBATIM
            )
            set(target_deps ${target_deps} ${avm_name}-${jit_target_arch_variant}.avm)
        endforeach()
    endif()
    add_custom_command(
        OUTPUT ${avm_name}-pico.uf2
        DEPENDS ${avm_name}.avm UF2Tool
        COMMAND ${CMAKE_BINARY_DIR}/tools/uf2tool/uf2tool create -o ${avm_name}-pico.uf2 -s 0x10100000 ${avm_name}.avm
        COMMENT "Creating UF2 file ${avm_name}.uf2"
        VERBATIM
    )
    add_custom_command(
        OUTPUT ${avm_name}-pico2.uf2
        DEPENDS ${avm_name}.avm UF2Tool
        COMMAND ${CMAKE_BINARY_DIR}/tools/uf2tool/uf2tool create -o ${avm_name}-pico2.uf2 -f data -s 0x10100000 ${avm_name}.avm
        COMMENT "Creating UF2 file ${avm_name}.uf2"
        VERBATIM
    )
    set(target_deps ${target_deps} ${avm_name}-pico.uf2 ${avm_name}-pico2.uf2)

    if(NOT AVM_DISABLE_JIT OR AVM_ENABLE_PRECOMPILED)
        add_custom_command(
            OUTPUT ${avm_name}-armv6m-pico.uf2
            DEPENDS ${avm_name}-armv6m.avm UF2Tool
            COMMAND ${CMAKE_BINARY_DIR}/tools/uf2tool/uf2tool create -o ${avm_name}-armv6m-pico.uf2 -s 0x10100000 ${avm_name}-armv6m.avm
            COMMENT "Creating UF2 file ${avm_name}-armv6m.uf2"
            VERBATIM
        )
        add_custom_command(
            OUTPUT ${avm_name}-armv6m-pico2.uf2
            DEPENDS ${avm_name}-armv6m.avm UF2Tool
            COMMAND ${CMAKE_BINARY_DIR}/tools/uf2tool/uf2tool create -o ${avm_name}-armv6m-pico2.uf2 -f data -s 0x10100000 ${avm_name}-armv6m.avm
            COMMENT "Creating UF2 file ${avm_name}-armv6m.uf2"
            VERBATIM
        )
        set(target_deps ${target_deps} ${avm_name}-armv6m-pico.uf2 ${avm_name}-armv6m-pico2.uf2)
    endif()

    add_custom_target(
        ${avm_name} ALL
        DEPENDS ${target_deps}
    )
    if(TARGET ${avm_name}_main)
        add_dependencies(${avm_name} ${avm_name}_main)
    endif()

endmacro()


macro(pack_runnable avm_name main)

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
            set(pack_runnable_${avm_name}_archives ${pack_runnable_${avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}.avm)
            if(NOT ${archive_name} MATCHES "^eavmlib|estdlib|alisp$")
                set(${avm_name}_dialyzer_beams_opt ${${avm_name}_dialyzer_beams_opt} "-r" ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/beams/)
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

    add_custom_command(
        OUTPUT ${avm_name}.avm
        DEPENDS ${avm_name}_main ${pack_runnable_${avm_name}_archive_targets} PackBEAM
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM ${INCLUDE_LINES} ${avm_name}.avm ${main}.beam ${pack_runnable_${avm_name}_archives}
        COMMENT "Packing runnable ${avm_name}.avm"
        VERBATIM
    )

    add_custom_target(
        ${avm_name} ALL
        DEPENDS ${avm_name}.avm
    )
endmacro()


macro(pack_test test_avm_name)
    set(pack_test_${test_avm_name}_archives ${CMAKE_CURRENT_BINARY_DIR}/${test_avm_name}_lib.avm)
    set(pack_test_${test_avm_name}_archive_targets ${test_avm_name}_lib)

    if(AVM_DISABLE_JIT)
        set(precompiled_suffix "")
    else()
        set(precompiled_suffix "-${AVM_JIT_TARGET_ARCH}")
        set(pack_test_${test_avm_name}_archives ${pack_test_${test_avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/jit/src/jit${precompiled_suffix}.avm)
        set(pack_test_${test_avm_name}_archive_targets ${pack_test_${test_avm_name}_archive_targets} jit)
    endif()
    foreach(archive_name ${ARGN})
        if(${archive_name} MATCHES "^estdlib$")
            set(pack_test_${test_avm_name}_archives ${pack_test_${test_avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}${precompiled_suffix}.avm)
        else()
            set(pack_test_${test_avm_name}_archives ${pack_test_${test_avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}.avm)
        endif()
        set(pack_test_${test_avm_name}_archive_targets ${pack_test_${test_avm_name}_archive_targets} ${archive_name})
    endforeach()

    if(AVM_RELEASE)
        set(INCLUDE_LINES "")
    else()
        set(INCLUDE_LINES "-i")
    endif()

    add_custom_command(
        OUTPUT tests.beam
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/tests.erl
        COMMAND erlc +debug_info -I ${CMAKE_SOURCE_DIR}/libs/include ${CMAKE_CURRENT_SOURCE_DIR}/tests.erl
        VERBATIM
    )

    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${test_avm_name}.avm
        DEPENDS ${pack_test_${test_avm_name}_archive_targets} PackBEAM tests.beam
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM ${INCLUDE_LINES} ${CMAKE_CURRENT_BINARY_DIR}/${test_avm_name}.avm ${CMAKE_CURRENT_BINARY_DIR}/tests.beam ${pack_test_${test_avm_name}_archives}
        COMMENT "Packing runnable ${test_avm_name}.avm"
        VERBATIM
    )

    add_custom_target(
        ${test_avm_name} ALL
        DEPENDS ${test_avm_name}.avm
    )
endmacro()

macro(pack_eunit test_avm_name)
    set(pack_eunit_${test_avm_name}_archives ${CMAKE_CURRENT_BINARY_DIR}/${test_avm_name}_lib.avm)
    set(pack_eunit_${test_avm_name}_archive_targets ${test_avm_name}_lib)

    if(AVM_DISABLE_JIT)
        set(precompiled_suffix "")
    else()
        set(precompiled_suffix "-${AVM_JIT_TARGET_ARCH}")
        set(pack_eunit_${test_avm_name}_archives ${pack_eunit_${test_avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/jit/src/jit${precompiled_suffix}.avm)
        set(pack_eunit_${test_avm_name}_archive_targets ${pack_eunit_${test_avm_name}_archive_targets} jit)
    endif()
    foreach(archive_name ${ARGN})
        if(${archive_name} MATCHES "^estdlib$")
            set(pack_eunit_${test_avm_name}_archives ${pack_eunit_${test_avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}${precompiled_suffix}.avm)
        else()
            set(pack_eunit_${test_avm_name}_archives ${pack_eunit_${test_avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}.avm)
        endif()
        set(pack_eunit_${test_avm_name}_archive_targets ${pack_eunit_${test_avm_name}_archive_targets} ${archive_name})
    endforeach()

    if(AVM_RELEASE)
        set(INCLUDE_LINES "")
    else()
        set(INCLUDE_LINES "-i")
    endif()

    add_custom_command(
        OUTPUT ${test_avm_name}.avm
        DEPENDS ${pack_eunit_${test_avm_name}_archive_targets} PackBEAM ${CMAKE_BINARY_DIR}/libs/etest/src/beams/eunit.beam
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM ${INCLUDE_LINES} ${CMAKE_CURRENT_BINARY_DIR}/${test_avm_name}.avm ${CMAKE_BINARY_DIR}/libs/etest/src/beams/eunit.beam ${pack_eunit_${test_avm_name}_archives}
        COMMENT "Packing runnable ${test_avm_name}.avm"
        VERBATIM
    )

    add_custom_target(
        ${test_avm_name} ALL
        DEPENDS ${test_avm_name}.avm etest
    )
endmacro()

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
            set(pack_uf2_${avm_name}_archives ${pack_uf2_${avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}.avm)
        else()
            set(pack_uf2_${avm_name}_archives ${pack_uf2_${avm_name}_archives} ${CMAKE_BINARY_DIR}/libs/${archive_name}/lib/${archive_name}.avm)
        endif()
        set(pack_uf2_${avm_name}_archive_targets ${pack_uf2_${avm_name}_archive_targets} ${archive_name})
    endforeach()

    add_custom_command(
        OUTPUT ${avm_name}.avm
        DEPENDS ${avm_name}_main ${pack_uf2_${avm_name}_archive_targets} PackBEAM
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM ${avm_name}.avm ${main}.beam ${pack_uf2_${avm_name}_archives}
        COMMENT "Packing runnable ${avm_name}.avm"
        VERBATIM
    )

    add_custom_command(
        OUTPUT ${avm_name}.uf2
        DEPENDS ${avm_name}.avm UF2Tool
        COMMAND ${CMAKE_BINARY_DIR}/tools/uf2tool/uf2tool create -o ${avm_name}.uf2 -f universal -s 0x10180000 ${avm_name}.avm
        COMMENT "Creating UF2 file ${avm_name}.uf2"
        VERBATIM
    )

    add_custom_target(
        ${avm_name} ALL
        DEPENDS ${avm_name}.uf2
    )
endmacro()
