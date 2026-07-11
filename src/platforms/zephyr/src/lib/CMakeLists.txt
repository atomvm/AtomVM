#
# This file is part of AtomVM.
#
# Copyright 2022 Paul Guyot <pguyot@kallisys.net>
# Copyright 2023 Winford <winford@object.stream>
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

cmake_minimum_required (VERSION 3.20.5)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)
project (libAtomVM${PLATFORM_LIB_SUFFIX})
zephyr_library_named(libAtomVM${PLATFORM_LIB_SUFFIX})

zephyr_include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_BINARY_DIR}
    ../../../../libAtomVM
    ${CMAKE_BINARY_DIR}/zephyr/include/generated/
)

if(CONFIG_FAT_FILESYSTEM_ELM)
    zephyr_include_directories(
        ${ZEPHYR_BASE}/../modules/fs/fatfs/include
    )
endif()
if(CONFIG_POSIX_API)
    zephyr_compile_definitions(HAVE_OPEN HAVE_CLOSE HAVE_READDIR HAVE_OPENDIR HAVE_CLOSEDIR HAVE_PATH_MAX O_NOCTTY=0 O_SYNC=0)
    target_compile_definitions(libAtomVM PUBLIC HAVE_OPEN HAVE_CLOSE HAVE_READDIR HAVE_OPENDIR HAVE_CLOSEDIR HAVE_PATH_MAX O_NOCTTY=0 O_SYNC=0)
endif()

if(HAVE_PLATFORM_SMP_H)
    zephyr_library_compile_definitions(HAVE_PLATFORM_SMP_H)
endif()
if(HAVE_PLATFORM_ATOMIC_H)
    zephyr_library_compile_definitions(HAVE_PLATFORM_ATOMIC_H)
endif()
if(ATOMIC_POINTER_LOCK_FREE_IS_TWO)
    zephyr_library_compile_definitions(HAVE_ATOMIC)
endif()

zephyr_library_sources(
    platform_nifs.c
    sys.c
)

if(CONFIG_PM_POLICY_CUSTOM)
    zephyr_library_sources(
        pm_policy.c
    )
endif()

if(NOT AVM_DISABLE_SMP)
    zephyr_library_sources(
        smp.c
    )
endif()

if(CONFIG_NET_SOCKETS)
    zephyr_library_sources(
        otp_socket_platform.c
        ../../../../libAtomVM/otp_socket.c
        ../../../../libAtomVM/inet.c
        otp_net_platform.c
        ../../../../libAtomVM/otp_net.c
    )
endif()

if(CONFIG_PSA_CRYPTO)
    zephyr_library_sources(
        ../../../../libAtomVM/otp_crypto.c
    )
    if(CONFIG_NET_SOCKETS)
        zephyr_library_sources(
            ../../../../libAtomVM/otp_ssl.c
        )
    endif()
endif()


if(CONFIG_WIFI)
    zephyr_library_sources(
        network_driver.c
    )
endif()

if(CONFIG_SERIAL)
    zephyr_library_sources(
        uart_driver.c
    )
endif()

if(CONFIG_I2C)
    zephyr_library_sources(
        i2c_driver.c
    )
endif()

if(CONFIG_SPI)
    zephyr_library_sources(
        spi_driver.c
    )
endif()

if(CONFIG_PWM)
    zephyr_library_sources(
        pwm_driver.c
    )
endif()

if(CONFIG_RETENTION)
    zephyr_library_sources(
        retention_driver.c
    )
endif()

if(CONFIG_ADC)
    zephyr_library_sources(
        adc_driver.c
    )
endif()

if(CONFIG_GPIO)
    zephyr_library_sources(
        gpio_driver.c
    )
endif()

if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC -Wall -pedantic -Wextra -ggdb)
endif()
set_property(TARGET libAtomVM${PLATFORM_LIB_SUFFIX} PROPERTY C_STANDARD 11)

target_link_libraries(app PUBLIC libAtomVM${PLATFORM_LIB_SUFFIX})
target_link_options(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ${LINK_OPTIONS} -Wl,--whole-archive ${CMAKE_CURRENT_BINARY_DIR}/liblibAtomVM${PLATFORM_LIB_SUFFIX}.a -Wl,--no-whole-archive)
