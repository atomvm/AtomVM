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

project(examples_erlang_stm32)

include(BuildErlang)

# STM32 examples bundle runtime libs in the same AVM and targets are flash-constrained.
set(AVM_PRUNE_RUNNABLES ON)

pack_runnable(blink_weact_studio_blackpill blink_weact_studio_blackpill eavmlib avm_stm32)
pack_runnable(blink_weact_studio_h562 blink_weact_studio_h562 eavmlib avm_stm32)
pack_runnable(blink_weact_studio_h743 blink_weact_studio_h743 eavmlib avm_stm32)
pack_runnable(blink_weact_studio_u585 blink_weact_studio_u585 eavmlib avm_stm32)
pack_runnable(blink_weact_studio_wb55 blink_weact_studio_wb55 eavmlib avm_stm32)
pack_runnable(blink_nucleo64 blink_nucleo64 eavmlib avm_stm32)
pack_runnable(blink_nucleo144 blink_nucleo144 eavmlib avm_stm32)

if(AVM_RELEASE)
    set(INCLUDE_LINES "--remove_lines")
else()
    set(INCLUDE_LINES "")
endif()

set(SPI_LIS3DH_BEAM ${CMAKE_BINARY_DIR}/examples/erlang/spi_lis3dh.beam)
add_custom_command(
    OUTPUT stm32_spi_lis3dh.avm
    DEPENDS spi_lis3dh_main ${SPI_LIS3DH_BEAM}
        ${CMAKE_BINARY_DIR}/libs/estdlib/src/estdlib.avm estdlib
        ${CMAKE_BINARY_DIR}/libs/avm_stm32/src/avm_stm32.avm avm_stm32
        PackBEAM
    COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/packbeam create ${INCLUDE_LINES} stm32_spi_lis3dh.avm
        ${SPI_LIS3DH_BEAM}
        ${CMAKE_BINARY_DIR}/libs/estdlib/src/estdlib.avm
        ${CMAKE_BINARY_DIR}/libs/avm_stm32/src/avm_stm32.avm
    COMMENT "Packing runnable stm32_spi_lis3dh.avm"
    VERBATIM
)
add_custom_target(stm32_spi_lis3dh ALL DEPENDS stm32_spi_lis3dh.avm)

set(SPI_FLASH_BEAM ${CMAKE_BINARY_DIR}/examples/erlang/spi_flash.beam)
add_custom_command(
    OUTPUT stm32_spi_flash.avm
    DEPENDS spi_flash_main ${SPI_FLASH_BEAM}
        ${CMAKE_BINARY_DIR}/libs/estdlib/src/estdlib.avm estdlib
        ${CMAKE_BINARY_DIR}/libs/avm_stm32/src/avm_stm32.avm avm_stm32
        PackBEAM
    COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/packbeam create ${INCLUDE_LINES} stm32_spi_flash.avm
        ${SPI_FLASH_BEAM}
        ${CMAKE_BINARY_DIR}/libs/estdlib/src/estdlib.avm
        ${CMAKE_BINARY_DIR}/libs/avm_stm32/src/avm_stm32.avm
    COMMENT "Packing runnable stm32_spi_flash.avm"
    VERBATIM
)
add_custom_target(stm32_spi_flash ALL DEPENDS stm32_spi_flash.avm)

set(SIM800L_BEAM ${CMAKE_BINARY_DIR}/examples/erlang/sim800l.beam)
add_custom_command(
    OUTPUT stm32_sim800l.avm
    DEPENDS sim800l_main ${SIM800L_BEAM}
        ${CMAKE_BINARY_DIR}/libs/estdlib/src/estdlib.avm estdlib
        ${CMAKE_BINARY_DIR}/libs/avm_stm32/src/avm_stm32.avm avm_stm32
        PackBEAM
    COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/packbeam create ${INCLUDE_LINES} stm32_sim800l.avm
        ${SIM800L_BEAM}
        ${CMAKE_BINARY_DIR}/libs/estdlib/src/estdlib.avm
        ${CMAKE_BINARY_DIR}/libs/avm_stm32/src/avm_stm32.avm
    COMMENT "Packing runnable stm32_sim800l.avm"
    VERBATIM
)
add_custom_target(stm32_sim800l ALL DEPENDS stm32_sim800l.avm)
