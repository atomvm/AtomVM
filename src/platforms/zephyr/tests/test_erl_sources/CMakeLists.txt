#
# This file is part of AtomVM.
#
# Copyright 2026 Peter M <petermm@gmail.com>
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

include(ExternalProject)

get_filename_component(ATOMVM_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/../../../../.." ABSOLUTE)

ExternalProject_Add(HostAtomVM
    SOURCE_DIR "${ATOMVM_ROOT}"
    CMAKE_ARGS -DAVM_DISABLE_JIT=ON
    INSTALL_COMMAND cmake -E echo "Skipping install step."
    BUILD_COMMAND cmake --build . --target=atomvmlib-zephyr --target=PackBEAM
)

function(compile_erlang module_name)
    add_custom_command(
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.beam"
        COMMAND erlc "${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl"
        DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl"
        COMMENT "Compiling ${module_name}.erl"
        WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
        VERBATIM
    )
    set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.beam")
endfunction()

compile_erlang(test_list_to_atom)
compile_erlang(test_i2c_native_sim)

set(erlang_test_beams
    test_list_to_atom.beam
    test_i2c_native_sim.beam
)


add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/zephyr_test_modules.avm"
    COMMAND HostAtomVM-prefix/src/HostAtomVM-build/tools/packbeam/packbeam create zephyr_test_modules.avm
        ${erlang_test_beams}
        HostAtomVM-prefix/src/HostAtomVM-build/libs/atomvmlib-zephyr.avm
    DEPENDS
        HostAtomVM
        ${erlang_test_beams}
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    VERBATIM
)

set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/zephyr_test_modules.avm")

add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/zephyr_test_modules.avm.c"
    COMMAND "${CMAKE_COMMAND}"
        -D "DATA_FILE=${CMAKE_CURRENT_BINARY_DIR}/zephyr_test_modules.avm"
        -D "SOURCE_FILE=${CMAKE_CURRENT_BINARY_DIR}/zephyr_test_modules.avm.c"
        -D "SYMBOL=zephyr_test_modules_avm"
        -P "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/EmbedBinaryAsC.cmake"
    MAIN_DEPENDENCY "${CMAKE_CURRENT_BINARY_DIR}/zephyr_test_modules.avm"
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/EmbedBinaryAsC.cmake"
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    VERBATIM
)

set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/zephyr_test_modules.avm.c")

set_source_files_properties("${CMAKE_CURRENT_BINARY_DIR}/zephyr_test_modules.avm.c" PROPERTIES GENERATED TRUE)
add_custom_target(zephyr_test_modules_avm_c DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/zephyr_test_modules.avm.c")
add_dependencies(app zephyr_test_modules_avm_c)
target_sources(app PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/zephyr_test_modules.avm.c")
