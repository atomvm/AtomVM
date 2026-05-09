#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

# Fetch TinyUSB and create an interface library for STM32 USB CDC support.
# Only the device-side CDC class on the synopsys/dwc2 controller is wired up.
# Selects MCU define + DCD port based on STM32_FAMILY_SHORT.

include(FetchContent)

if (DEFINED ENV{TINYUSB_PATH} AND (NOT FETCHCONTENT_SOURCE_DIR_TINYUSB))
    set(FETCHCONTENT_SOURCE_DIR_TINYUSB $ENV{TINYUSB_PATH})
    message("Using TINYUSB_PATH from environment ('${FETCHCONTENT_SOURCE_DIR_TINYUSB}')")
endif()

FetchContent_Declare(
    tinyusb
    GIT_REPOSITORY https://github.com/hathach/tinyusb.git
    GIT_TAG        0.18.0
    GIT_SHALLOW    TRUE
)
FetchContent_GetProperties(tinyusb)
if (NOT tinyusb_POPULATED)
    message("Downloading TinyUSB")
endif()
FetchContent_MakeAvailable(tinyusb)

set(TINYUSB_SRC_DIR "${tinyusb_SOURCE_DIR}/src")

# Map STM32 family -> TinyUSB MCU define + DCD source files.
# H7/F2/F4/F7: synopsys dwc2 OTG controller.
# H5: STM32 USB_DRD_FS device controller (the "stm32_fsdev" port in TinyUSB).
set(_DWC2_SOURCES
    "${TINYUSB_SRC_DIR}/portable/synopsys/dwc2/dcd_dwc2.c"
    "${TINYUSB_SRC_DIR}/portable/synopsys/dwc2/dwc2_common.c"
)
set(_FSDEV_SOURCES
    "${TINYUSB_SRC_DIR}/portable/st/stm32_fsdev/dcd_stm32_fsdev.c"
)

if (STM32_FAMILY_SHORT STREQUAL "h7")
    set(STM32_TINYUSB_MCU "OPT_MCU_STM32H7")
    set(STM32_TINYUSB_DCD_SOURCES ${_DWC2_SOURCES})
elseif (STM32_FAMILY_SHORT STREQUAL "h5")
    set(STM32_TINYUSB_MCU "OPT_MCU_STM32H5")
    set(STM32_TINYUSB_DCD_SOURCES ${_FSDEV_SOURCES})
else()
    message(FATAL_ERROR "AVM_USB_CDC_PORT_DRIVER_ENABLED: TinyUSB MCU mapping not configured for STM32 family ${STM32_FAMILY_SHORT}")
endif()

# Core TinyUSB sources required for device CDC, plus the family-specific DCD.
set(TINYUSB_SOURCES
    "${TINYUSB_SRC_DIR}/tusb.c"
    "${TINYUSB_SRC_DIR}/common/tusb_fifo.c"
    "${TINYUSB_SRC_DIR}/device/usbd.c"
    "${TINYUSB_SRC_DIR}/device/usbd_control.c"
    "${TINYUSB_SRC_DIR}/class/cdc/cdc_device.c"
    ${STM32_TINYUSB_DCD_SOURCES}
)

add_library(tinyusb_device STATIC ${TINYUSB_SOURCES})
target_include_directories(tinyusb_device PUBLIC
    "${TINYUSB_SRC_DIR}"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/lib"  # for tusb_config.h
)
target_compile_definitions(tinyusb_device PUBLIC
    "CFG_TUSB_MCU=${STM32_TINYUSB_MCU}"
    "${STM32_HAL_DEVICE}"
)
target_include_directories(tinyusb_device SYSTEM PUBLIC
    ${CMSIS_CORE_INCLUDE}
    ${CMSIS_DEVICE_INCLUDE}
    ${HAL_DRIVER_INCLUDE}
    "${CMAKE_BINARY_DIR}/generated"
)
# TinyUSB has plenty of casts / fall-through that aren't worth hardening here.
target_compile_options(tinyusb_device PRIVATE
    -Wno-unused-parameter
    -Wno-implicit-fallthrough
    -Wno-cast-align
    -Wno-sign-compare
)

message(STATUS "TinyUSB MCU   : ${STM32_TINYUSB_MCU}")
message(STATUS "TinyUSB DCD   : ${STM32_TINYUSB_DCD_PORT}")
