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

# Parse DEVICE string to determine STM32 family, CPU flags, HAL defines, memory sizes.
# Input: DEVICE (e.g. "stm32f401ccu6", "stm32h743vit6", "stm32u585ait6q", "stm32wb55rg", "stm32h562rgt6")
# Output variables:
#   STM32_FAMILY       - full family string (e.g. "stm32f4xx")
#   STM32_FAMILY_UPPER - upper case (e.g. "STM32F4xx")
#   STM32_FAMILY_UC    - all upper case (e.g. "STM32F4XX")
#   STM32_FAMILY_SHORT - two-char family (e.g. "f4")
#   STM32_CPU          - CPU core (e.g. "cortex-m4")
#   STM32_FPU          - FPU type (e.g. "fpv4-sp-d16") or empty
#   STM32_ARCH_FLAGS   - list of architecture flags

if (NOT DEVICE)
    message(FATAL_ERROR "No DEVICE specified")
endif()

# Normalize to lowercase
string(TOLOWER ${DEVICE} DEVICE_LOWER)

# Extract family: e.g. stm32f401ccu6 -> "f4"
string(SUBSTRING ${DEVICE_LOWER} 5 2 STM32_FAMILY_SHORT)

# Family data: CPU, FPU, float ABI
# Format: set(_FAM_<family>  "<cpu>;<fpu>;<float_abi>")
set(_FAM_f2  "cortex-m3;;soft")
set(_FAM_f4  "cortex-m4;fpv4-sp-d16;hard")
set(_FAM_f7  "cortex-m7;fpv5-d16;hard")
set(_FAM_g0  "cortex-m0plus;;soft")
set(_FAM_g4  "cortex-m4;fpv4-sp-d16;hard")
set(_FAM_h5  "cortex-m33;fpv5-sp-d16;hard")
set(_FAM_h7  "cortex-m7;fpv5-d16;hard")
set(_FAM_l4  "cortex-m4;fpv4-sp-d16;hard")
set(_FAM_l5  "cortex-m33;fpv5-sp-d16;hard")
set(_FAM_u3  "cortex-m33;fpv5-sp-d16;hard")
set(_FAM_u5  "cortex-m33;fpv5-sp-d16;hard")
set(_FAM_wb  "cortex-m4;fpv4-sp-d16;hard")

if (NOT DEFINED _FAM_${STM32_FAMILY_SHORT})
    message(FATAL_ERROR "Unsupported STM32 family: ${STM32_FAMILY_SHORT} (from device ${DEVICE})")
endif()

# Unpack family data
list(GET _FAM_${STM32_FAMILY_SHORT} 0 STM32_CPU)
list(GET _FAM_${STM32_FAMILY_SHORT} 1 STM32_FPU)
list(GET _FAM_${STM32_FAMILY_SHORT} 2 STM32_FLOAT_ABI)

# Derive family name variants from the short code
string(TOUPPER ${STM32_FAMILY_SHORT} _fam_upper)
set(STM32_FAMILY "stm32${STM32_FAMILY_SHORT}xx")
set(STM32_FAMILY_UPPER "STM32${_fam_upper}xx")
set(STM32_FAMILY_UC "STM32${_fam_upper}XX")

# Build architecture flags
set(STM32_ARCH_FLAGS "-mcpu=${STM32_CPU}" "-mthumb" "-mthumb-interwork")
if (STM32_FPU)
    list(APPEND STM32_ARCH_FLAGS "-mfloat-abi=${STM32_FLOAT_ABI}" "-mfpu=${STM32_FPU}")
else()
    list(APPEND STM32_ARCH_FLAGS "-msoft-float")
endif()

# Apply architecture flags to compiler
string(REPLACE ";" " " _arch_flags_str "${STM32_ARCH_FLAGS}")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${_arch_flags_str}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${_arch_flags_str}")

# Add family-level define for stm32_hal_platform.h header selection
add_compile_definitions(${STM32_FAMILY_UC})
add_compile_definitions(USE_HAL_DRIVER)

# Linker script variables — defaults for most families
set(LINKER_RAM_ORIGIN "0x20000000")
set(LINKER_RAM_SIZE_OVERRIDE "")
set(LINKER_EXTRA_MEMORY "")
set(LINKER_EXTRA_SYMBOLS "")
set(LINKER_EXTRA_SECTIONS "")

# Family-specific linker overrides
if (STM32_FAMILY_SHORT STREQUAL "f7")
    set(LINKER_RAM_ORIGIN "0x20010000")
    set(LINKER_EXTRA_MEMORY "DTCMRAM (rw)  : ORIGIN = 0x20000000, LENGTH = 64K")
elseif (STM32_FAMILY_SHORT STREQUAL "h7")
    set(LINKER_RAM_ORIGIN "0x24000000")
    set(LINKER_EXTRA_MEMORY "DTCMRAM (rw)  : ORIGIN = 0x20000000, LENGTH = 128K")
elseif (STM32_FAMILY_SHORT STREQUAL "u3")
    set(LINKER_EXTRA_SYMBOLS "_sstack = _estack - _Min_Stack_Size;")
elseif (STM32_FAMILY_SHORT STREQUAL "wb")
    set(LINKER_RAM_SIZE_OVERRIDE "192")
    set(LINKER_EXTRA_MEMORY "RAM_SHARED (rwx) : ORIGIN = 0x20030000, LENGTH = 10K")
    set(LINKER_EXTRA_SECTIONS [=[
    /* Mailbox shared memory (SRAM2a) for CPU1/CPU2 IPC */
    _siMB_MEM2 = LOADADDR(.MB_MEM2);

    .MB_MEM2 :
    {
        . = ALIGN(4);
        _sMB_MEM2 = .;
        *(MB_MEM2)
        . = ALIGN(4);
        _eMB_MEM2 = .;
    } >RAM_SHARED AT> FLASH]=])
endif()

message("-----------Device Info-----------")
message(STATUS "Device      : ${DEVICE}")
message(STATUS "Family      : ${STM32_FAMILY}")
message(STATUS "CPU         : ${STM32_CPU}")
message(STATUS "FPU         : ${STM32_FPU}")
message(STATUS "Arch Flags  : ${_arch_flags_str}")
