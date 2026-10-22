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
project(libAtomVMPlatformRTEMS)

set(HEADER_FILES
    adc_driver.h
    flash_driver.h
    flash_fs.h
    ledc_driver.h
    network.h
    nvs_driver.h
    rtc.h
    sntp.h
    wdt.h
    wifi.h
    rtems_sys.h
    ../../../../libAtomVM/platform_nifs.h
    ../../../../libAtomVM/portnifloader.h
    ../../../../libAtomVM/sys.h
)

set(SOURCE_FILES
    adc_driver.c
    flash_driver.c
    flash_fs.c
    ledc_driver.c
    network.c
    network_driver.c
    network_nifs.c
    nvs_driver.c
    sntp.c
    wdt.c
    wifi.c
    platform_nifs.c
    sys.c
    uart_driver.c
    i2c_driver.c
    gpio_driver.c
    spi_driver.c
    rtc_slow_nif.c
    esp_sleep.c
    esp_sleep_hw.c
    smp.c
    ../../../../libAtomVM/portnifloader.c
)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

if(RTEMS_SLEEP_TEST_FIXTURE)
    list(REMOVE_ITEM SOURCE_FILES esp_sleep_hw.c)
endif()

# The BLE controller reuses the Wi-Fi image copy and coexistence layer, so it is
# only built for C3/C5 with the ESP Wi-Fi stack installed.
if(RTEMS_BT AND RTEMS_BOARD STREQUAL "esp32c3" AND RTEMS_HAS_ESP_WIFI)
    list(APPEND SOURCE_FILES rtems_bt.c)
    list(APPEND HEADER_FILES rtems_bt.h)
elseif(RTEMS_BT AND RTEMS_BOARD STREQUAL "esp32c5" AND RTEMS_HAS_ESP_WIFI)
    set(_rtems_bt_dir "${CMAKE_CURRENT_SOURCE_DIR}/../../bt")
    list(APPEND SOURCE_FILES "${_rtems_bt_dir}/rtems_bt_c5.c" "${_rtems_bt_dir}/rtems_npl.c" ble_vhci_nif.c)
    list(APPEND HEADER_FILES rtems_bt.h rtems_bt_vhci.h)
endif()

add_library(libAtomVM${PLATFORM_LIB_SUFFIX} ${SOURCE_FILES} ${HEADER_FILES})
target_compile_features(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC c_std_11)
if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC -Wall -pedantic -Wextra -ggdb)
endif()

target_include_directories(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC libAtomVM)
# Constructor-registered NIF collections and port drivers are otherwise dropped from the static archive.
set(_rtems_force_nifs uart_nif i2c_nif gpio_nif spi_nif rtems_network_nif networkregister_port_driver rtc_slow_nif wdt_nif ledc_nif adc_nif flash_nif nvs_nif flash_fs_nif)
if(RTEMS_BT AND RTEMS_BOARD STREQUAL "esp32c5" AND RTEMS_HAS_ESP_WIFI)
    list(APPEND _rtems_force_nifs ble_vhci_nif)
endif()
if(RTEMS_HAS_LIBBSD OR RTEMS_HAS_LWIP)
    list(APPEND _rtems_force_nifs otp_socket_nif otp_net_nif)
endif()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeModules")
find_package(MbedTLS QUIET)
if(NOT MbedTLS_FOUND)
    include(MbedTLS)
endif()
if(MbedTLS_FOUND)
    target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ATOMVM_HAS_MBEDTLS HAVE_MBEDTLS=1)
    target_sources(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE
        otp_crypto_platform.c
        ../../../../libAtomVM/otp_crypto.c
        ../../../../libAtomVM/otp_crypto.h
    )
    target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC MbedTLS::mbedcrypto)
    list(APPEND _rtems_force_nifs otp_crypto_nif)

    if(RTEMS_HAS_LIBBSD OR RTEMS_HAS_LWIP)
        target_sources(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE
            otp_ssl_platform.c
            ../../../../libAtomVM/otp_ssl.c
            ../../../../libAtomVM/otp_ssl.h
        )
        target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC
            MbedTLS::mbedtls
            MbedTLS::mbedx509
        )
        list(APPEND _rtems_force_nifs otp_ssl_nif)
    endif()
endif()

set(_rtems_force_link)
foreach(_nif ${_rtems_force_nifs})
    list(APPEND _rtems_force_link "SHELL:-Wl,-u -Wl,${_nif}")
endforeach()
target_link_options(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ${_rtems_force_link})

if(RTEMS_ARCH STREQUAL "arm" AND RTEMS_BSP_NAME STREQUAL "imx7")
    target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE
        RTEMS_HAS_GPIO
        RTEMS_HAS_IMX_GPIO
        RTEMS_HAS_I2C
        RTEMS_HAS_IMX_I2C
        RTEMS_HAS_SPI
        RTEMS_HAS_IMX_SPI
    )
    if(RTEMS_BOARD STREQUAL "grisp2")
        target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE
            RTEMS_HAS_WIFI
        )
    endif()
elseif(RTEMS_BOARD STREQUAL "esp32c3" OR RTEMS_BOARD STREQUAL "esp32c5" OR RTEMS_BOARD STREQUAL "esp32c6" OR RTEMS_BOARD STREQUAL "esp32s31")
    target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE
        RTEMS_BOARD_ESP32
        RTEMS_HAS_GPIO
        RTEMS_HAS_ESP32C3_GPIO
        RTEMS_HAS_I2C
        RTEMS_HAS_ESP32C3_I2C
        RTEMS_HAS_SPI
        RTEMS_HAS_ESP32C3_SPI
    )
    if(RTEMS_BOARD STREQUAL "esp32c3")
        target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE RTEMS_BOARD_ESP32C3)
        if(RTEMS_BT)
            target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE RTEMS_HAS_BT)
        endif()
    elseif(RTEMS_BOARD STREQUAL "esp32c5")
        target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE RTEMS_BOARD_ESP32C5)
        if(RTEMS_BT AND RTEMS_HAS_ESP_WIFI)
            target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE RTEMS_HAS_BT)
            # bt/include provides the RTEMS NPL function table and wrappers.
            # The vendor controller headers stay in the BSP build; the
            # app-side glue declares only what it calls.
            target_include_directories(libAtomVM${PLATFORM_LIB_SUFFIX} BEFORE PRIVATE
                "${_rtems_bt_dir}/include"
            )
        endif()
    elseif(RTEMS_BOARD STREQUAL "esp32c6")
        target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE RTEMS_BOARD_ESP32C6)
    elseif(RTEMS_BOARD STREQUAL "esp32s31")
        target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE RTEMS_BOARD_ESP32S31)
    endif()
    target_include_directories(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE
        "${CMAKE_CURRENT_SOURCE_DIR}/include"
    )
    if(RTEMS_HAS_ESP_WIFI)
        target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE
            RTEMS_HAS_WIFI
            RTEMS_WIFI_BACKEND_ESP32
        )
        if(NOT RTEMS_WIFI_POWER_SAVE_MODE STREQUAL "NONE")
            target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE
                RTEMS_WIFI_POWER_SAVE=WIFI_PS_${RTEMS_WIFI_POWER_SAVE_MODE}
                RTEMS_WIFI_LISTEN_INTERVAL=${RTEMS_WIFI_LISTEN_INTERVAL}
            )
        endif()
        # Use the SDK's board-specific configuration before the local C3 fallback.
        target_include_directories(libAtomVM${PLATFORM_LIB_SUFFIX} BEFORE PRIVATE
            "${RTEMS_PREFIX}/${RTEMS_TARGET}/${RTEMS_BSP_NAME}/lib/include/rtems-esp"
        )
    endif()
endif()

if(RTEMS_HAS_LIBBSD OR RTEMS_HAS_LWIP)
    target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC
        HAVE_SOCKET=1
        HAVE_SELECT=1
        HAVE_GETHOSTNAME
    )
    target_sources(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE
        otp_socket_platform.c
        otp_socket_platform.h
        ../../../../libAtomVM/inet.c
        ../../../../libAtomVM/otp_net.c
        ../../../../libAtomVM/otp_socket.c
    )
    if(RTEMS_HAS_LIBBSD)
        target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC RTEMS_HAS_LIBBSD)
        target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC bsd)
    elseif(RTEMS_HAS_LWIP)
        target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC RTEMS_HAS_LWIP)
        target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC lwip)
    endif()
endif()
