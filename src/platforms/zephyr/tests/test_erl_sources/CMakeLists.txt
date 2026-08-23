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
    BUILD_COMMAND cmake --build . --target=atomvmlib-zephyr --target=esp32devmode --target=PackBEAM
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
compile_erlang(test_adc)
compile_erlang(test_gpio)
compile_erlang(test_i2c_native_sim)
compile_erlang(test_system_architecture)
compile_erlang(test_list_to_binary)
compile_erlang(test_monotonic_time)
compile_erlang(test_time_and_processes)
compile_erlang(test_tz)
compile_erlang(test_mount)
compile_erlang(test_md5)
compile_erlang(test_crypto)
compile_erlang(test_file)
compile_erlang(test_select)
compile_erlang(test_spi)
compile_erlang(test_pwm)
compile_erlang(test_dac)
compile_erlang(test_wifi_ap)
compile_erlang(test_devmode)
compile_erlang(test_wifi_scan)
compile_erlang(test_wifi_managed)
compile_erlang(test_wifi_example)
compile_erlang(test_net)
compile_erlang(test_retention)
compile_erlang(test_ssl)
compile_erlang(test_deep_sleep)
compile_erlang(test_deep_sleep_hold)
compile_erlang(test_twdt)
compile_erlang(test_uart)
compile_erlang(test_platform)
compile_erlang(test_settings)

set(erlang_test_beams
    test_list_to_atom.beam
    test_adc.beam
    test_gpio.beam
    test_i2c_native_sim.beam
    test_system_architecture.beam
    test_list_to_binary.beam
    test_monotonic_time.beam
    test_time_and_processes.beam
    test_tz.beam
    test_mount.beam
    test_md5.beam
    test_crypto.beam
    test_file.beam
    test_select.beam
    test_spi.beam
    test_pwm.beam
    test_dac.beam
    test_wifi_ap.beam
    test_devmode.beam
    test_wifi_scan.beam
    test_wifi_managed.beam
    test_wifi_example.beam
    test_net.beam
    test_retention.beam
    test_ssl.beam
    test_deep_sleep.beam
    test_deep_sleep_hold.beam
    test_twdt.beam
    test_uart.beam
    test_platform.beam
    test_settings.beam
)


add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/zephyr_test_modules.avm"
    COMMAND HostAtomVM-prefix/src/HostAtomVM-build/tools/packbeam/packbeam create zephyr_test_modules.avm
        ${erlang_test_beams}
        HostAtomVM-prefix/src/HostAtomVM-build/libs/atomvmlib-zephyr.avm
        HostAtomVM-prefix/src/HostAtomVM-build/libs/esp32devmode/src/esp32devmode.avm
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
