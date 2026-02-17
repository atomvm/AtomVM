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

*** Settings ***
Suite Setup       Setup
Suite Teardown    Teardown
Test Setup        Reset Emulation
Resource          ${RENODEKEYWORDS}

*** Variables ***
${UART}           sysbus.usart1
${PLATFORM}       @platforms/cpus/stm32f4.repl
${ELF}            REQUIRED
${AVM}            REQUIRED
${AVM_ADDRESS}    0x08060000

*** Test Cases ***
AtomVM Should Drive GPIO Pin
    Execute Command    mach create
    Execute Command    machine LoadPlatformDescription ${PLATFORM}
    Execute Command    machine LoadPlatformDescriptionFromString "led: Miscellaneous.LED @ gpioPortA"
    Execute Command    machine LoadPlatformDescriptionFromString "gpioPortA: { 5 -> led@0 }"
    Execute Command    sysbus LoadELF ${ELF}
    Execute Command    sysbus LoadBinary ${AVM} ${AVM_ADDRESS}
    Create Terminal Tester    ${UART}
    Create LED Tester    sysbus.gpioPortA.led
    Start Emulation
    Wait For Line On Uart    gpio_done    timeout=60
    Assert LED State    true    timeout=1
