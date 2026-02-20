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

project(examples_erlang_rp2040)

include(BuildErlang)

pack_uf2(hello_pico hello_pico)
pack_uf2(pico_blink pico_blink)
pack_uf2(pico_i2c_scanner pico_i2c_scanner)
pack_uf2(pico_lis3dh pico_lis3dh)
pack_uf2(pico_rtc pico_rtc)

set(SPI_FLASH_AVM ${CMAKE_BINARY_DIR}/examples/erlang/spi_flash.avm)
add_custom_command(
    OUTPUT spi_flash.uf2
    DEPENDS ${SPI_FLASH_AVM} UF2Tool
    COMMAND ${CMAKE_BINARY_DIR}/tools/uf2tool/uf2tool create -o spi_flash.uf2 -f universal -s 0x10180000 ${SPI_FLASH_AVM}
    COMMENT "Creating UF2 file spi_flash.uf2"
    VERBATIM
)
add_custom_target(spi_flash_uf2 ALL DEPENDS spi_flash.uf2)
add_dependencies(spi_flash_uf2 spi_flash)
pack_uf2(picow_blink picow_blink)
pack_uf2(picow_wifi_sta picow_wifi_sta)
pack_uf2(picow_wifi_ap picow_wifi_ap)
pack_uf2(picow_udp_beacon picow_udp_beacon)
pack_uf2(picow_tcp_server picow_tcp_server)
pack_uf2(picow_wifi_epmd_disterl picow_wifi_epmd_disterl)
