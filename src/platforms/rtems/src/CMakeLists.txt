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

if(RTEMS_BOARD STREQUAL "grisp2")
    set(PROJECT_EXECUTABLE ${PROJECT_NAME}-${RTEMS_BOARD}.exe)
else()
    set(PROJECT_EXECUTABLE ${PROJECT_NAME}-${RTEMS_BSP_NAME}.exe)
endif()

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

set(_rtems_sources
    main.c
    app_loader.c
    rtems_config.c
    ${EMBEDDED_AVM_C}
)
if(RTEMS_HAS_LIBBSD)
    list(APPEND _rtems_sources rtems_bsd_config.c)
endif()

add_executable(${PROJECT_EXECUTABLE}
    ${_rtems_sources}
)

if(RTEMS_BOARD STREQUAL "grisp2")
    set(RTEMS_APP_PATH "/media/mmcsd-0-0/app.avm" CACHE STRING
        "RTEMS filesystem path to the replaceable AVM application")
    target_sources(${PROJECT_EXECUTABLE} PRIVATE storage.c)
    target_compile_definitions(${PROJECT_EXECUTABLE} PRIVATE
        RTEMS_USE_BSP_NEXUS
        RTEMS_HAS_REGFIX
        RTEMS_HAS_KSZ8091RNB
        RTEMS_HAS_STORAGE
        RTEMS_HAS_WIFI
        RTEMS_BOARD_FDT_COMPATIBLE="${RTEMS_BOARD_FDT_COMPATIBLE}"
        RTEMS_APP_PATH="${RTEMS_APP_PATH}")
elseif(RTEMS_BOARD STREQUAL "esp32c3")
    target_compile_definitions(${PROJECT_EXECUTABLE} PRIVATE RTEMS_BOARD_ESP32 RTEMS_BOARD_ESP32C3)
elseif(RTEMS_BOARD STREQUAL "esp32c5")
    target_compile_definitions(${PROJECT_EXECUTABLE} PRIVATE RTEMS_BOARD_ESP32 RTEMS_BOARD_ESP32C5)
elseif(RTEMS_BOARD STREQUAL "esp32c6")
    target_compile_definitions(${PROJECT_EXECUTABLE} PRIVATE RTEMS_BOARD_ESP32 RTEMS_BOARD_ESP32C6)
elseif(RTEMS_BOARD STREQUAL "esp32s31")
    target_compile_definitions(${PROJECT_EXECUTABLE} PRIVATE RTEMS_BOARD_ESP32 RTEMS_BOARD_ESP32S31)
elseif(RTEMS_ARCH STREQUAL "arm" AND RTEMS_BSP_NAME STREQUAL "realview_pbx_a9_qemu")
    target_compile_definitions(${PROJECT_EXECUTABLE} PRIVATE RTEMS_BSP_REALVIEW_PBX_A9_QEMU)
    target_link_options(${PROJECT_EXECUTABLE} PRIVATE
        -Wl,--wrap=bsp_start_hook_0
        -Wl,--wrap=bsp_start_hook_1
    )
endif()

option(RTEMS_SLEEP_TEST_FIXTURE "Replace RTC/PMU hardware for the sleep integration test only" OFF)
if(RTEMS_SLEEP_TEST_FIXTURE)
    if(NOT RTEMS_BOARD MATCHES "^esp32")
        message(FATAL_ERROR "The sleep fixture requires an ESP32 BSP")
    endif()
    target_sources(${PROJECT_EXECUTABLE} PRIVATE ../tests/sleep_fixture.c)
endif()

target_compile_features(${PROJECT_EXECUTABLE} PUBLIC c_std_11)
if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(${PROJECT_EXECUTABLE} PUBLIC -Wall -pedantic -Wextra -ggdb)
endif()

include(SystemArchitecture)
# The esp32 riscv toolchain reports "riscv-rtems7", which normalizes to "riscv".
# Override to "riscv32" and tag the vendor as "esp" so the ESP32 boards mirror
# the ESP-IDF form (e.g. riscv32-esp-esp_idf) with the RTEMS runtime in slot 3.
if(RTEMS_BOARD MATCHES "^esp32")
    avm_get_system_architecture_string(AVM_SYSTEM_ARCHITECTURE_STRING PLATFORM_ARCH riscv32 PLATFORM_VENDOR esp PLATFORM_OS rtems)
else()
    avm_get_system_architecture_string(AVM_SYSTEM_ARCHITECTURE_STRING PLATFORM_VENDOR rtems PLATFORM_OS rtems)
endif()
add_subdirectory(../../../libAtomVM libAtomVM)
if(HAVE_PLATFORM_ATOMIC_H)
    target_include_directories(libAtomVM PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/lib")
endif()
target_link_libraries(${PROJECT_EXECUTABLE} PUBLIC libAtomVM)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

add_subdirectory(lib)
target_link_libraries(${PROJECT_EXECUTABLE} PRIVATE libAtomVM${PLATFORM_LIB_SUFFIX})
if(RTEMS_HAS_LIBBSD)
    target_compile_definitions(${PROJECT_EXECUTABLE} PRIVATE RTEMS_HAS_LIBBSD HAVE_SOCKET=1 HAVE_SELECT=1)
    target_link_libraries(${PROJECT_EXECUTABLE} PRIVATE bsd)
endif()

if(RTEMS_HAS_LWIP)
    target_compile_definitions(${PROJECT_EXECUTABLE} PRIVATE RTEMS_HAS_LWIP HAVE_SOCKET=1 HAVE_SELECT=1)
    target_link_libraries(${PROJECT_EXECUTABLE} PRIVATE lwip)
endif()

if(RTEMS_BOARD STREQUAL "esp32c3" AND RTEMS_HAS_ESP_WIFI)
    target_compile_definitions(${PROJECT_EXECUTABLE} PRIVATE RTEMS_HAS_WIFI RTEMS_WIFI_BACKEND_ESP32)
    if(RTEMS_BT)
        target_compile_definitions(${PROJECT_EXECUTABLE} PRIVATE RTEMS_HAS_BT)
        set(_esp32c3_bt_libs btdm_app_flash btbb)
    endif()
    set(_esp32c3_lib_dir "${RTEMS_PREFIX}/${RTEMS_TARGET}/${RTEMS_BSP_NAME}/lib")
    target_link_options(${PROJECT_EXECUTABLE} PRIVATE
        -Wl,-u,esp32c3_netif_add
        -Wl,-u,esp_supplicant_init
        -Wl,-u,g_wifi_default_wpa_crypto_funcs
        -Wl,-T,${_esp32c3_lib_dir}/esp32c3.rom.ld
        -Wl,-T,${_esp32c3_lib_dir}/esp32c3.rom.api.ld
        -Wl,-T,${_esp32c3_lib_dir}/esp32c3.rom.version.ld
        -Wl,-T,${_esp32c3_lib_dir}/esp32c3.rom.eco3.ld
    )
    target_link_libraries(${PROJECT_EXECUTABLE} PRIVATE
        -Wl,--start-group
        rtems_esp_wifi
        lwip
        wpa
        mbedtls
        net80211
        pp
        core
        phy
        coexist
        ${_esp32c3_bt_libs}
        mesh
        espnow
        smartconfig
        wapi
        jffs2
        z
        -Wl,--end-group
    )
elseif(RTEMS_BOARD STREQUAL "esp32c6")
    if(RTEMS_HAS_ESP_WIFI)
        target_compile_definitions(${PROJECT_EXECUTABLE} PRIVATE RTEMS_HAS_WIFI RTEMS_WIFI_BACKEND_ESP32)
        set(_esp32c6_lib_dir "${RTEMS_PREFIX}/${RTEMS_TARGET}/${RTEMS_BSP_NAME}/lib")
        target_link_options(${PROJECT_EXECUTABLE} PRIVATE
            -Wl,--no-relax-gp
            -Wl,-u,esp32c6_netif_add
            -Wl,-u,esp_supplicant_init
            -Wl,-u,g_wifi_default_wpa_crypto_funcs
            -Wl,-T,${_esp32c6_lib_dir}/esp32c6.rom.ld
            -Wl,-T,${_esp32c6_lib_dir}/esp32c6.rom.api.ld
            -Wl,-T,${_esp32c6_lib_dir}/esp32c6.rom.version.ld
            -Wl,-T,${_esp32c6_lib_dir}/esp32c6.rom.phy.ld
            -Wl,-T,${_esp32c6_lib_dir}/esp32c6.rom.net80211.ld
            -Wl,-T,${_esp32c6_lib_dir}/esp32c6.rom.pp.ld
            -Wl,-T,${_esp32c6_lib_dir}/esp32c6.rom.coexist.ld
            -Wl,-T,${CMAKE_CURRENT_SOURCE_DIR}/../ld/esp32c6.rom.ld
        )
        target_link_libraries(${PROJECT_EXECUTABLE} PRIVATE
            -Wl,--start-group
            rtems_esp_wifi
            lwip
            wpa
            mbedtls
            net80211
            pp
            core
            phy
            coexist
            mesh
            espnow
            smartconfig
            wapi
            jffs2
            z
            -Wl,--end-group
        )
    else()
        target_link_options(${PROJECT_EXECUTABLE} PRIVATE
            -Wl,-T,${CMAKE_CURRENT_SOURCE_DIR}/../ld/esp32c6.rom.ld
        )
        target_link_libraries(${PROJECT_EXECUTABLE} PRIVATE
            jffs2
            z
        )
    endif()
elseif(RTEMS_BOARD STREQUAL "esp32c5")
    if(RTEMS_HAS_ESP_WIFI)
        set(RTEMS_ESP32C5_REV_MIN 100 CACHE STRING
            "Minimum ESP32-C5 chip revision (100 for ECO2, 102 for ECO3)")
        target_compile_definitions(${PROJECT_EXECUTABLE} PRIVATE RTEMS_HAS_WIFI RTEMS_WIFI_BACKEND_ESP32)
        if(RTEMS_BT)
            target_compile_definitions(${PROJECT_EXECUTABLE} PRIVATE RTEMS_HAS_BT)
            set(_esp32c5_bt_libs ble_app btbb)
        endif()
        set(_esp32c5_lib_dir "${RTEMS_PREFIX}/${RTEMS_TARGET}/${RTEMS_BSP_NAME}/lib")
        # Installed archives and scripts can change while the application
        # sources stay unchanged during BSP and Wi-Fi bringup.
        file(GLOB _esp32c5_link_inputs
            "${_esp32c5_lib_dir}/*.a"
            "${_esp32c5_lib_dir}/esp32c5.rom*.ld"
            "${_esp32c5_lib_dir}/linkcmds*"
        )
        set_property(TARGET ${PROJECT_EXECUTABLE} APPEND PROPERTY LINK_DEPENDS
            ${_esp32c5_link_inputs}
            "${CMAKE_CURRENT_SOURCE_DIR}/../ld/esp32c5.rom.ld"
        )
        target_link_options(${PROJECT_EXECUTABLE} PRIVATE
            -Wl,--no-relax-gp
            -Wl,-u,esp32c5_netif_add
            -Wl,-u,esp_supplicant_init
            -Wl,-u,g_wifi_default_wpa_crypto_funcs
            -Wl,-T,${_esp32c5_lib_dir}/esp32c5.rom.ld
            -Wl,-T,${_esp32c5_lib_dir}/esp32c5.rom.api.ld
            -Wl,-T,${_esp32c5_lib_dir}/esp32c5.rom.version.ld
            -Wl,-T,${_esp32c5_lib_dir}/esp32c5.rom.phy.ld
            -Wl,-T,${_esp32c5_lib_dir}/esp32c5.rom.net80211.ld
            -Wl,-T,${_esp32c5_lib_dir}/esp32c5.rom.pp.ld
            -Wl,-T,${_esp32c5_lib_dir}/esp32c5.rom.coexist.ld
            -Wl,-T,${CMAKE_CURRENT_SOURCE_DIR}/../ld/esp32c5.rom.ld
        )
        if(RTEMS_ESP32C5_REV_MIN GREATER_EQUAL 102)
            target_link_options(${PROJECT_EXECUTABLE} PRIVATE
                -Wl,-T,${_esp32c5_lib_dir}/esp32c5.rom.eco3.ld
            )
        endif()
        target_link_libraries(${PROJECT_EXECUTABLE} PRIVATE
            -Wl,--start-group
            rtems_esp_wifi
            lwip
            wpa
            mbedtls
            net80211
            pp
            core
            phy
            coexist
            ${_esp32c5_bt_libs}
            mesh
            espnow
            smartconfig
            wapi
            jffs2
            z
            -Wl,--end-group
        )
    else()
        target_link_options(${PROJECT_EXECUTABLE} PRIVATE
            -Wl,-T,${CMAKE_CURRENT_SOURCE_DIR}/../ld/esp32c5.rom.ld
        )
        target_link_libraries(${PROJECT_EXECUTABLE} PRIVATE
            jffs2
            z
        )
    endif()
elseif(RTEMS_BOARD STREQUAL "esp32s31")
    if(RTEMS_HAS_ESP_WIFI)
        set(RTEMS_ESP32S31_REV_MIN 0 CACHE STRING
            "Minimum ESP32-S31 chip revision")
        target_compile_definitions(${PROJECT_EXECUTABLE} PRIVATE RTEMS_HAS_WIFI RTEMS_WIFI_BACKEND_ESP32)
        set(_esp32s31_lib_dir "${RTEMS_PREFIX}/${RTEMS_TARGET}/${RTEMS_BSP_NAME}/lib")
        file(GLOB _esp32s31_link_inputs
            "${_esp32s31_lib_dir}/*.a"
            "${_esp32s31_lib_dir}/esp32s31.rom*.ld"
            "${_esp32s31_lib_dir}/linkcmds*"
        )
        set_property(TARGET ${PROJECT_EXECUTABLE} APPEND PROPERTY LINK_DEPENDS
            ${_esp32s31_link_inputs}
            "${CMAKE_CURRENT_SOURCE_DIR}/../ld/esp32s31.rom.ld"
        )
        target_link_options(${PROJECT_EXECUTABLE} PRIVATE
            -Wl,--no-relax-gp
            -Wl,-u,esp32s31_netif_add
            -Wl,-u,esp_supplicant_init
            -Wl,-u,g_wifi_default_wpa_crypto_funcs
            -Wl,-T,${_esp32s31_lib_dir}/esp32s31.rom.ld
            -Wl,-T,${_esp32s31_lib_dir}/esp32s31.rom.api.ld
            -Wl,-T,${_esp32s31_lib_dir}/esp32s31.rom.version.ld
            -Wl,-T,${_esp32s31_lib_dir}/esp32s31.rom.phy.ld
            -Wl,-T,${_esp32s31_lib_dir}/esp32s31.rom.net80211.ld
            -Wl,-T,${_esp32s31_lib_dir}/esp32s31.rom.pp.ld
            -Wl,-T,${_esp32s31_lib_dir}/esp32s31.rom.coexist.ld
            -Wl,-T,${CMAKE_CURRENT_SOURCE_DIR}/../ld/esp32s31.rom.ld
        )
        target_link_libraries(${PROJECT_EXECUTABLE} PRIVATE
            -Wl,--start-group
            rtems_esp_wifi
            lwip
            wpa
            mbedtls
            net80211
            pp
            core
            phy
            coexist
            mesh
            espnow
            smartconfig
            wapi
            jffs2
            z
            -Wl,--end-group
        )
    else()
        target_link_options(${PROJECT_EXECUTABLE} PRIVATE
            -Wl,-T,${CMAKE_CURRENT_SOURCE_DIR}/../ld/esp32s31.rom.ld
        )
        target_link_libraries(${PROJECT_EXECUTABLE} PRIVATE
            jffs2
            z
        )
    endif()
elseif(RTEMS_BOARD MATCHES "esp32")
    target_link_libraries(${PROJECT_EXECUTABLE} PRIVATE
        jffs2
        z
    )
endif()

if(RTEMS_SIZE)
    add_custom_command(
        TARGET ${PROJECT_EXECUTABLE}
        POST_BUILD
        COMMAND ${RTEMS_SIZE} ${PROJECT_EXECUTABLE}
        WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
    )
endif()

if(RTEMS_CREATE_ZIMAGE)
    include(${CMAKE_CURRENT_SOURCE_DIR}/../cmake/boot_image.cmake)
endif()

if(RTEMS_BOARD MATCHES "esp32")
    include(${CMAKE_CURRENT_SOURCE_DIR}/../cmake/esp32_image.cmake)
endif()

message("------------Output Locations------------")
message(STATUS "EXE: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${PROJECT_EXECUTABLE}")
if(AVM_PACK)
    message(STATUS "AVM: ${AVM_PACK}")
else()
    message(STATUS "AVM: (none embedded; executable will refuse to boot)")
endif()
