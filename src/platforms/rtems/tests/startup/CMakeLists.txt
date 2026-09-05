# Copyright 2026 Peter M. <petermm@gmail.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

# Linux host integration test: real VM and RTEMS boot logic, stubbed board I/O.
set(_startup_pack "${CMAKE_CURRENT_BINARY_DIR}/test_startup.avm")
set(_startup_embedded "${CMAKE_CURRENT_BINARY_DIR}/embedded_avm.c")
add_custom_command(
    OUTPUT ${_startup_pack}
    COMMAND ${ERLC_PATH} -o ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/test_startup.erl
    COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/packbeam create ${_startup_pack} ${CMAKE_CURRENT_BINARY_DIR}/test_startup.beam
    DEPENDS test_startup.erl PackBEAM
    VERBATIM
)
add_custom_command(
    OUTPUT ${_startup_embedded}
    COMMAND ${CMAKE_COMMAND} -DAVM_IN=${_startup_pack} -DAVM_OUT=${_startup_embedded}
        -P ${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/embed_avm.cmake
    DEPENDS ${_startup_pack} ../../cmake/embed_avm.cmake
    VERBATIM
)
add_executable(test-rtems-startup EXCLUDE_FROM_ALL test_startup.c
    app_heap.c ../../src/app_loader.c
    ../../../../libAtomVM/portnifloader.c ${_startup_embedded})
target_include_directories(test-rtems-startup PRIVATE . ../../src)
target_compile_definitions(test-rtems-startup PRIVATE RTEMS_HAS_STORAGE)
target_compile_features(test-rtems-startup PRIVATE c_std_11)
target_compile_options(test-rtems-startup PRIVATE -Wall -Wextra -Werror)
target_link_libraries(test-rtems-startup PRIVATE libAtomVM libAtomVM${PLATFORM_LIB_SUFFIX})
target_link_options(test-rtems-startup PRIVATE
    -Wl,--wrap=malloc,--wrap=calloc,--wrap=realloc,--wrap=free,--wrap=strdup
    -Wl,--wrap=module_new_from_iff_binary)
