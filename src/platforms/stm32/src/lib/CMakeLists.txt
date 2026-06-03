#
# This file is part of AtomVM.
#
# Copyright 2022-2026 Paul Guyot <pguyot@kallisys.net>
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
project (libAtomVMPlatformSTM32)

if (STM32_HAS_RNG)
    # Skip mbedTLS programs/tests that pull in POSIX-only code (sockets,
    # stdio, etc.) and won't link on bare metal anyway.
    set(ENABLE_PROGRAMS OFF CACHE BOOL "" FORCE)
    set(ENABLE_TESTING OFF CACHE BOOL "" FORCE)

    include(FetchMbedTLS)

    # Apply our user config only to the mbedtls targets, not to the rest
    # of the firmware.
    set(_mbedtls_stm32_cfg "MBEDTLS_USER_CONFIG_FILE=\"${CMAKE_CURRENT_SOURCE_DIR}/mbedtls_stm32_user_config.h\"")
    foreach(_tgt mbedcrypto mbedtls mbedx509)
        if(TARGET ${_tgt})
            target_compile_definitions(${_tgt} PRIVATE "${_mbedtls_stm32_cfg}")
        endif()
    endforeach()
    unset(_mbedtls_stm32_cfg)
    unset(_tgt)
endif()

set(HEADER_FILES
    avm_devcfg.h
    avm_log.h
    gpio_driver.h
    stm_sys.h
    ../../../../libAtomVM/platform_nifs.h
    ../../../../libAtomVM/portnifloader.h
    ../../../../libAtomVM/sys.h
)

set(SOURCE_FILES
    gpio_driver.c
    i2c_driver.c
    spi_driver.c
    uart_driver.c
    jit_stream_flash.c
    platform_nifs.c
    sys.c
    ../../../../libAtomVM/portnifloader.c
)

if (STM32_HAS_RNG)
    list(APPEND HEADER_FILES
        ../../../../libAtomVM/otp_crypto.h)
    list(APPEND SOURCE_FILES
        ../../../../libAtomVM/otp_crypto.c
        otp_crypto_platform.c)
endif()

if (AVM_USB_CDC_PORT_DRIVER_ENABLED)
    list(APPEND SOURCE_FILES usb_cdc_driver.c usb_descriptors.c usb_irq_handlers.c usb_hw_init.c)
endif()

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

add_library(libAtomVM${PLATFORM_LIB_SUFFIX} ${SOURCE_FILES} ${HEADER_FILES})
target_compile_features(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC c_std_11)
if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC -Wall -pedantic -Wextra -ggdb)
endif()

target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC libAtomVM)
if (STM32_HAS_RNG)
    target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ATOMVM_HAS_MBEDTLS)
    target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC
        "MBEDTLS_USER_CONFIG_FILE=\"${CMAKE_CURRENT_SOURCE_DIR}/mbedtls_stm32_user_config.h\"")
    target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC MbedTLS::mbedcrypto)
endif()
target_link_options(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ${LINK_OPTIONS} -Wl,--whole-archive ${CMAKE_CURRENT_BINARY_DIR}/liblibAtomVMGeneric-arm.a -Wl,--no-whole-archive)

# Include HAL/CMSIS headers
target_include_directories(libAtomVM${PLATFORM_LIB_SUFFIX} SYSTEM PUBLIC
    ${CMSIS_CORE_INCLUDE}
    ${CMSIS_DEVICE_INCLUDE}
    ${HAL_DRIVER_INCLUDE}
)

# Include the generated hal_conf.h directory
target_include_directories(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC
    "${CMAKE_BINARY_DIR}/generated"
)

if (AVM_USB_CDC_PORT_DRIVER_ENABLED)
    target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE
        AVM_USB_CDC_PORT_DRIVER_ENABLED
        USBD_VID=${AVM_USB_CDC_VID}
        USBD_PID=${AVM_USB_CDC_PID}
        "USBD_MANUFACTURER=\"${AVM_USB_CDC_MANUFACTURER}\""
        "USBD_PRODUCT=\"${AVM_USB_CDC_PRODUCT}\""
        "USBD_INTERFACE_NAME=\"${AVM_USB_CDC_INTERFACE_NAME}\""
    )
    target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC tinyusb_device)
    target_link_options(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC "SHELL:-Wl,-u -Wl,usb_cdcregister_port_driver")
endif()
