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
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

# Fetch STM32 SDK components via FetchContent:
#   1. CMSIS core (ARM CMSIS headers, shared across all families)
#   2. CMSIS device (family-specific device headers + startup files)
#   3. HAL driver (family-specific HAL + LL drivers)

include(FetchContent)

# Allow using local source checkouts via environment variables or CMake variables.
# CMSIS Core is shared across all families; CMSIS Device and HAL Driver are
# family-specific, so env var names include the family (e.g. STM32_CMSIS_DEVICE_F4_PATH).
if (DEFINED ENV{STM32_CMSIS_CORE_PATH} AND (NOT FETCHCONTENT_SOURCE_DIR_CMSIS_CORE))
    set(FETCHCONTENT_SOURCE_DIR_CMSIS_CORE $ENV{STM32_CMSIS_CORE_PATH})
    message("Using STM32_CMSIS_CORE_PATH from environment ('${FETCHCONTENT_SOURCE_DIR_CMSIS_CORE}')")
endif ()

string(TOUPPER ${STM32_FAMILY_SHORT} _FAMILY_UPPER)

set(_cmsis_device_env_var "STM32_CMSIS_DEVICE_${_FAMILY_UPPER}_PATH")
if (DEFINED ENV{${_cmsis_device_env_var}} AND (NOT FETCHCONTENT_SOURCE_DIR_CMSIS_DEVICE))
    set(FETCHCONTENT_SOURCE_DIR_CMSIS_DEVICE "$ENV{${_cmsis_device_env_var}}")
    message("Using ${_cmsis_device_env_var} from environment ('${FETCHCONTENT_SOURCE_DIR_CMSIS_DEVICE}')")
endif ()

set(_hal_driver_env_var "STM32_HAL_DRIVER_${_FAMILY_UPPER}_PATH")
if (DEFINED ENV{${_hal_driver_env_var}} AND (NOT FETCHCONTENT_SOURCE_DIR_HAL_DRIVER))
    set(FETCHCONTENT_SOURCE_DIR_HAL_DRIVER "$ENV{${_hal_driver_env_var}}")
    message("Using ${_hal_driver_env_var} from environment ('${FETCHCONTENT_SOURCE_DIR_HAL_DRIVER}')")
endif ()

# CMSIS Core - common ARM headers
FetchContent_Declare(
    cmsis_core
    GIT_REPOSITORY https://github.com/STMicroelectronics/cmsis_core.git
    GIT_TAG        v5.9.0
    GIT_SHALLOW    TRUE
)
FetchContent_GetProperties(cmsis_core)
if (NOT cmsis_core_POPULATED)
    message("Downloading CMSIS Core headers")
endif()
FetchContent_MakeAvailable(cmsis_core)

# Family-specific SDK versions.
# Format: set(_SDK_<family>  "<cmsis_device_tag>;<hal_driver_tag>")
# Repos follow the naming convention:
#   CMSIS device: https://github.com/STMicroelectronics/cmsis_device_<family>.git
#   HAL driver:   https://github.com/STMicroelectronics/stm32<family>xx_hal_driver.git
# Exceptions are handled with _SDK_<family>_CMSIS_REPO / _SDK_<family>_HAL_REPO overrides.
set(_SDK_f2  "v2.2.6;v1.2.9")
set(_SDK_f4  "v2.6.10;v1.8.3")
set(_SDK_f7  "v1.2.10;v1.3.3")
set(_SDK_g0  "v1.4.4;v1.4.6")
set(_SDK_g4  "v1.2.6;v1.2.6")
set(_SDK_h5  "v1.2.0;v1.2.0")
set(_SDK_h7  "v1.10.4;v1.11.3")
set(_SDK_l4  "v1.7.5;v1.13.6")
set(_SDK_l5  "v1.0.7;v1.0.7")
set(_SDK_u3  "v1.3.0;v1.2.0")
set(_SDK_u5  "v1.4.0;v1.6.0")
set(_SDK_wb  "v1.12.2;v1.14.3")

# U3 uses hyphens instead of underscores in repo names
set(_SDK_u3_CMSIS_REPO "https://github.com/STMicroelectronics/cmsis-device-u3.git")
set(_SDK_u3_HAL_REPO   "https://github.com/STMicroelectronics/stm32u3xx-hal-driver.git")

if (NOT DEFINED _SDK_${STM32_FAMILY_SHORT})
    message(FATAL_ERROR "No SDK configuration for family: ${STM32_FAMILY_SHORT}")
endif()

list(GET _SDK_${STM32_FAMILY_SHORT} 0 _CMSIS_DEVICE_TAG)
list(GET _SDK_${STM32_FAMILY_SHORT} 1 _HAL_DRIVER_TAG)

# Use override repos if defined, otherwise use naming convention
if (DEFINED _SDK_${STM32_FAMILY_SHORT}_CMSIS_REPO)
    set(_CMSIS_DEVICE_REPO "${_SDK_${STM32_FAMILY_SHORT}_CMSIS_REPO}")
else()
    set(_CMSIS_DEVICE_REPO "https://github.com/STMicroelectronics/cmsis_device_${STM32_FAMILY_SHORT}.git")
endif()

if (DEFINED _SDK_${STM32_FAMILY_SHORT}_HAL_REPO)
    set(_HAL_DRIVER_REPO "${_SDK_${STM32_FAMILY_SHORT}_HAL_REPO}")
else()
    set(_HAL_DRIVER_REPO "https://github.com/STMicroelectronics/stm32${STM32_FAMILY_SHORT}xx_hal_driver.git")
endif()

FetchContent_Declare(
    cmsis_device
    GIT_REPOSITORY ${_CMSIS_DEVICE_REPO}
    GIT_TAG        ${_CMSIS_DEVICE_TAG}
    GIT_SHALLOW    TRUE
)
FetchContent_GetProperties(cmsis_device)
if (NOT cmsis_device_POPULATED)
    message("Downloading CMSIS Device headers for ${STM32_FAMILY_SHORT}")
endif()
FetchContent_MakeAvailable(cmsis_device)

FetchContent_Declare(
    hal_driver
    GIT_REPOSITORY ${_HAL_DRIVER_REPO}
    GIT_TAG        ${_HAL_DRIVER_TAG}
    GIT_SHALLOW    TRUE
)
FetchContent_GetProperties(hal_driver)
if (NOT hal_driver_POPULATED)
    message("Downloading HAL/LL drivers for ${STM32_FAMILY_SHORT}")
endif()
FetchContent_MakeAvailable(hal_driver)

# Export include directories for use by targets
set(CMSIS_CORE_INCLUDE "${cmsis_core_SOURCE_DIR}/Include")
set(CMSIS_DEVICE_INCLUDE "${cmsis_device_SOURCE_DIR}/Include")
set(HAL_DRIVER_INCLUDE "${hal_driver_SOURCE_DIR}/Inc")
set(HAL_DRIVER_SRC_DIR "${hal_driver_SOURCE_DIR}/Src")

# Find startup file for the device.
# Startup files use varied naming conventions:
#   startup_stm32f411xe.s  — wildcard package, specific flash code
#   startup_stm32f410cx.s  — specific package, wildcard flash
#   startup_stm32f407xx.s  — fully generic
# Extract the device line (4 chars from pos 5), package (pos 9), and flash code (pos 10).
if (NOT DEFINED DEVICE_LOWER)
    message(FATAL_ERROR "DEVICE_LOWER is not defined. It must be set by stm32_device.cmake before including stm32_sdk.cmake.")
endif()
string(SUBSTRING ${DEVICE_LOWER} 5 4 _dev_line)
string(LENGTH ${DEVICE_LOWER} _dev_len)
set(_startup_dir "${cmsis_device_SOURCE_DIR}/Source/Templates/gcc")
set(STM32_STARTUP_FILE "")
if (_dev_len GREATER 10)
    string(SUBSTRING ${DEVICE_LOWER} 9 1 _dev_package)
    string(SUBSTRING ${DEVICE_LOWER} 10 1 _dev_flash)
    # Try: wildcard package + specific flash (most common)
    file(GLOB STM32_STARTUP_FILE "${_startup_dir}/startup_stm32${_dev_line}x${_dev_flash}.s")
    # Try: specific package + wildcard flash
    if (NOT STM32_STARTUP_FILE)
        file(GLOB STM32_STARTUP_FILE "${_startup_dir}/startup_stm32${_dev_line}${_dev_package}x.s")
    endif()
endif()
# Try: fully generic (xx)
if (NOT STM32_STARTUP_FILE)
    file(GLOB STM32_STARTUP_FILE "${_startup_dir}/startup_stm32${_dev_line}xx.s")
endif()
# Last resort: any matching startup file for this device line
if (NOT STM32_STARTUP_FILE)
    file(GLOB STM32_STARTUP_FILE "${_startup_dir}/startup_stm32${_dev_line}*.s")
    if (STM32_STARTUP_FILE)
        list(GET STM32_STARTUP_FILE 0 STM32_STARTUP_FILE)
    endif()
endif()
if (NOT STM32_STARTUP_FILE)
    message(FATAL_ERROR "Startup file not found for ${DEVICE} in ${_startup_dir}/")
endif()

# Derive HAL device define from the startup filename.
# E.g. startup_stm32f411xe.s -> STM32F411xE
# Convention: 'x' stays lowercase (wildcard), other chars are uppercase.
get_filename_component(_startup_base ${STM32_STARTUP_FILE} NAME_WE)
string(REPLACE "startup_" "" _device_id ${_startup_base})
# Uppercase the device family/line part (first 9 chars: stm32 + 4-char line)
string(SUBSTRING ${_device_id} 0 9 _dev_prefix)
string(TOUPPER ${_dev_prefix} _dev_prefix)
# Handle the 2 variant chars: uppercase non-x, keep x lowercase
string(SUBSTRING ${_device_id} 9 1 _var1)
string(SUBSTRING ${_device_id} 10 1 _var2)
if (NOT _var1 STREQUAL "x")
    string(TOUPPER ${_var1} _var1)
endif()
if (NOT _var2 STREQUAL "x")
    string(TOUPPER ${_var2} _var2)
endif()
set(STM32_HAL_DEVICE "${_dev_prefix}${_var1}${_var2}")
add_compile_definitions(${STM32_HAL_DEVICE})

# System init template
set(STM32_SYSTEM_FILE "${cmsis_device_SOURCE_DIR}/Source/Templates/system_${STM32_FAMILY}.c")
if (NOT EXISTS ${STM32_SYSTEM_FILE})
    message(FATAL_ERROR "System file not found: ${STM32_SYSTEM_FILE}")
endif()

message("-----------SDK Info-----------")
message(STATUS "HAL Device    : ${STM32_HAL_DEVICE}")
message(STATUS "CMSIS Core    : ${cmsis_core_SOURCE_DIR}")
message(STATUS "CMSIS Device  : ${cmsis_device_SOURCE_DIR}")
message(STATUS "HAL Driver    : ${hal_driver_SOURCE_DIR}")
message(STATUS "Startup File  : ${STM32_STARTUP_FILE}")
message(STATUS "System File   : ${STM32_SYSTEM_FILE}")
