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
pack_uf2(pico_rtc pico_rtc)

set(I2C_SCANNER_AVM ${CMAKE_BINARY_DIR}/examples/erlang/i2c_scanner.avm)
add_custom_command(
    OUTPUT i2c_scanner.uf2
    DEPENDS ${I2C_SCANNER_AVM} UF2Tool
    COMMAND ${CMAKE_BINARY_DIR}/tools/uf2tool/uf2tool create -o i2c_scanner.uf2 -f universal -s 0x10180000 ${I2C_SCANNER_AVM}
    COMMENT "Creating UF2 file i2c_scanner.uf2"
    VERBATIM
)
add_custom_target(i2c_scanner_uf2 ALL DEPENDS i2c_scanner.uf2)
add_dependencies(i2c_scanner_uf2 i2c_scanner)

set(I2C_LIS3DH_AVM ${CMAKE_BINARY_DIR}/examples/erlang/i2c_lis3dh.avm)
add_custom_command(
    OUTPUT i2c_lis3dh.uf2
    DEPENDS ${I2C_LIS3DH_AVM} UF2Tool
    COMMAND ${CMAKE_BINARY_DIR}/tools/uf2tool/uf2tool create -o i2c_lis3dh.uf2 -f universal -s 0x10180000 ${I2C_LIS3DH_AVM}
    COMMENT "Creating UF2 file i2c_lis3dh.uf2"
    VERBATIM
)
add_custom_target(i2c_lis3dh_uf2 ALL DEPENDS i2c_lis3dh.uf2)
add_dependencies(i2c_lis3dh_uf2 i2c_lis3dh)

pack_uf2(picow_blink picow_blink)
pack_uf2(picow_wifi_sta picow_wifi_sta)
pack_uf2(picow_wifi_ap picow_wifi_ap)
pack_uf2(picow_udp_beacon picow_udp_beacon)
pack_uf2(picow_tcp_server picow_tcp_server)
pack_uf2(picow_wifi_epmd_disterl picow_wifi_epmd_disterl)
