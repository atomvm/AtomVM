#
# This file is part of AtomVM.
#
# Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

cmake_minimum_required (VERSION 3.13)

set(HEADER_FILES
    gpiodriver.h
    platform_defaultatoms.h
    platform_smp.h
    rp2040_sys.h
    ../../../../libAtomVM/otp_crypto.h
)

set(SOURCE_FILES
    gpiodriver.c
    networkdriver.c
    otp_crypto_platform.c
    platform_defaultatoms.c
    platform_nifs.c
    smp.c
    sys.c
    ../../../../libAtomVM/otp_crypto.c
)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

add_library(libAtomVM${PLATFORM_LIB_SUFFIX} STATIC ${SOURCE_FILES} ${HEADER_FILES})
target_compile_features(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC c_std_11)
if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE -Wall -pedantic -Wextra)
endif()

target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC libAtomVM)
target_link_libraries(
    libAtomVM${PLATFORM_LIB_SUFFIX}
    PUBLIC
        hardware_gpio
        hardware_rtc
        hardware_sync
        pico_float
        pico_mbedtls
        pico_multicore
        pico_platform
        pico_runtime
        pico_sync
        pico_time)

if (NOT AVM_USE_32BIT_FLOAT)
    target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC pico_double)
endif()

if (PICO_CYW43_SUPPORTED)
    set(BTSTACK_ROOT ${PICO_SDK_PATH}/lib/btstack)
    set(BTSTACK_3RD_PARTY_PATH ${BTSTACK_ROOT}/3rd-party)

    add_library(pan_lwip_dhserver INTERFACE)
    target_sources(pan_lwip_dhserver INTERFACE
        ${BTSTACK_3RD_PARTY_PATH}/lwip/dhcp-server/dhserver.c
    )
    target_include_directories(pan_lwip_dhserver INTERFACE
        ${BTSTACK_3RD_PARTY_PATH}/lwip/dhcp-server
    )
    target_compile_options(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE -DHAVE_LWIP_RAW=1)
    target_sources(
        libAtomVM${PLATFORM_LIB_SUFFIX}
        PRIVATE
            ../../../../libAtomVM/inet.c
            otp_socket_platform.c
            ../../../../libAtomVM/otp_socket.c
            otp_ssl_platform.c
            ../../../../libAtomVM/otp_ssl.c
            ../../../../libAtomVM/inet.h
            otp_socket_platform.h
            ../../../../libAtomVM/otp_socket.h
            ../../../../libAtomVM/otp_ssl.h
            otp_net_lwip_raw.c
            otp_net_lwip_raw.h)
    target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC pico_cyw43_arch_lwip_threadsafe_background pico_lwip_sntp INTERFACE pan_lwip_dhserver)
    target_link_options(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC "SHELL:-Wl,-u -Wl,networkregister_port_driver -Wl,-u -Wl,otp_socket_nif -Wl,-u -Wl,otp_net_nif -Wl,-u -Wl,otp_ssl_nif")
endif()

target_link_options(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC "SHELL:-Wl,-u -Wl,gpio_nif -Wl,-u -Wl,otp_crypto_nif")
