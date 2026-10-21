#
# This file is part of AtomVM.
#
# Copyright 2026 Peter M. <petermm@gmail.com>
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

# The RTEMS test runnables are only compiled to BEAM on the host; the RTEMS CI
# packs them into AVM packs with packbeam. They drive the board's NIF surface
# (gpio, ledc, esp, ...) and, like the ESP-IDF platform tests, are not packed or
# dialyzed as part of the host build.
function(rtems_test_beam avm_name main)
    add_custom_command(
        OUTPUT ${main}.beam
        COMMAND erlc +debug_info -I ${CMAKE_SOURCE_DIR}/libs/include ${CMAKE_CURRENT_SOURCE_DIR}/${main}.erl
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${main}.erl ${CMAKE_CURRENT_SOURCE_DIR}/wifi_scan.hrl
        COMMENT "Compiling ${main}.erl"
        VERBATIM
    )
    add_custom_target(${avm_name} DEPENDS ${main}.beam)
endfunction()

rtems_test_beam(rtems_boot_test test_boot)
rtems_test_beam(rtems_net_test test_net)
rtems_test_beam(rtems_grisp2_test test_grisp2)
rtems_test_beam(rtems_esp32c3_wifi_test test_esp32c3_wifi)
rtems_test_beam(rtems_esp32c3_ble_test test_esp32c3_ble)
rtems_test_beam(rtems_esp32c5_wifi_test test_esp32c5_wifi)
rtems_test_beam(rtems_esp32c5_ble_test test_esp32c5_ble)
rtems_test_beam(rtems_esp32c6_wifi_test test_esp32c6_wifi)
rtems_test_beam(rtems_esp32s31_wifi_test test_esp32s31_wifi)
rtems_test_beam(rtems_wokwi_wifi_test test_wokwi_wifi)
rtems_test_beam(rtems_smp_test test_smp)

rtems_test_beam(rtems_peripheral_test test_peripherals)
rtems_test_beam(rtems_deep_sleep_test test_deep_sleep)
