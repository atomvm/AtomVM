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

# Specify output executable
set(PROJECT_EXECUTABLE ${PROJECT_NAME}-${DEVICE}.elf)

# Collect HAL/LL source files we need
set(HAL_SOURCES
    ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal.c
    ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_rcc.c
    ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_rcc_ex.c
    ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_gpio.c
    ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_cortex.c
    ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_uart.c
    ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_flash.c
    ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_flash_ex.c
    ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_pwr.c
    ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_pwr_ex.c
    ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_exti.c
)

# DMA HAL (name varies: regular DMA for F4/H7/WB, GPDMA for U5/H5)
if (EXISTS "${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_dma.c")
    list(APPEND HAL_SOURCES ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_dma.c)
endif()
if (EXISTS "${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_gpdma.c")
    list(APPEND HAL_SOURCES ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_gpdma.c)
endif()

# I2C HAL
list(APPEND HAL_SOURCES ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_i2c.c)
if (EXISTS "${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_i2c_ex.c")
    list(APPEND HAL_SOURCES ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_i2c_ex.c)
endif()

# SPI HAL
list(APPEND HAL_SOURCES
    ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_spi.c
)
if (EXISTS "${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_spi_ex.c")
    list(APPEND HAL_SOURCES ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_spi_ex.c)
endif()

# ICACHE HAL for families that have it (U5, H5)
if (EXISTS "${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_icache.c")
    list(APPEND HAL_SOURCES ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_hal_icache.c)
endif()

# LL drivers for GPIO
set(LL_SOURCES
    ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_ll_gpio.c
    ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_ll_exti.c
)

# Check if ll_rcc exists (needed by some families)
if (EXISTS "${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_ll_rcc.c")
    list(APPEND LL_SOURCES ${HAL_DRIVER_SRC_DIR}/${STM32_FAMILY}_ll_rcc.c)
endif()

# Clock config for this family
set(CLOCK_CONFIG_SOURCE "${CMAKE_CURRENT_SOURCE_DIR}/../clock_configs/clock_config_${STM32_FAMILY_SHORT}.c")

add_executable(${PROJECT_EXECUTABLE}
    main.c
    ${CLOCK_CONFIG_SOURCE}
    ${STM32_SYSTEM_FILE}
    ${STM32_STARTUP_FILE}
    ${HAL_SOURCES}
    ${LL_SOURCES}
)

target_compile_features(${PROJECT_EXECUTABLE} PUBLIC c_std_11)
if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(${PROJECT_EXECUTABLE} PUBLIC -Wall -pedantic -Wextra -ggdb -std=gnu11)
endif()

# Startup file is assembly
set_source_files_properties(${STM32_STARTUP_FILE} PROPERTIES COMPILE_FLAGS "-x assembler-with-cpp")

# Include directories for HAL and CMSIS
target_include_directories(${PROJECT_EXECUTABLE} SYSTEM PUBLIC
    ${CMSIS_CORE_INCLUDE}
    ${CMSIS_DEVICE_INCLUDE}
    ${HAL_DRIVER_INCLUDE}
)

# Include the generated hal_conf.h directory and clock_config header
target_include_directories(${PROJECT_EXECUTABLE} PUBLIC
    "${CMAKE_CURRENT_BINARY_DIR}/../generated"
    "${CMAKE_CURRENT_SOURCE_DIR}/../clock_configs"
)

# Ensure picolibc is built before we compile any target (headers must exist)
add_dependencies(${PROJECT_EXECUTABLE} picolibc)

# Link libAtomVM
add_subdirectory(../../../libAtomVM libAtomVM)
add_dependencies(libAtomVM picolibc)
target_link_libraries(${PROJECT_EXECUTABLE} PUBLIC libAtomVM)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

add_subdirectory(lib)
target_link_libraries(${PROJECT_EXECUTABLE} PRIVATE libAtomVM${PLATFORM_LIB_SUFFIX})

# Link picolibc and libgcc (must come after application libs that reference libc symbols)
target_link_libraries(${PROJECT_EXECUTABLE} PRIVATE
    ${PICOLIBC_LIB_DIR}/libc.a
    gcc
    ${PICOLIBC_LIB_DIR}/libc.a
)

# Set linker flags
set_target_properties(${PROJECT_EXECUTABLE} PROPERTIES LINK_FLAGS "${LINKER_FLAGS}")

# Create binary output
add_custom_command(
    TARGET ${PROJECT_EXECUTABLE}
    POST_BUILD
    COMMAND ${ARM_OBJCOPY} -Obinary ${PROJECT_EXECUTABLE} ${PROJECT_NAME}-${DEVICE}.bin
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
)

# Output elf file size
add_custom_command(
    TARGET ${PROJECT_EXECUTABLE}
    POST_BUILD
    COMMAND ${ARM_SIZE} ${PROJECT_EXECUTABLE}
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
)

message("------------Output Locations------------")
message(STATUS "BIN: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${PROJECT_NAME}-${DEVICE}.bin")
message(STATUS "ELF: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${PROJECT_EXECUTABLE}")
