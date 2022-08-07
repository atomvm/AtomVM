#
# This file is part of AtomVM.
#
# Copyright 2022 Davide Bettio <davide@uninstall.it>
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

set(AVM_BUILTIN_COMPONENT_SRCS
    "gpio_driver.c"
    "i2c_driver.c"
    "ledc_nif.c"
    "network_driver.c"
    "nvs_nif.c"
    "socket_driver.c"
    "spi_driver.c"
    "uart_driver.c"
)

idf_component_register(SRCS ${AVM_BUILTIN_COMPONENT_SRCS}
    PRIV_REQUIRES "libatomvm" "main" "nvs_flash")

idf_build_set_property(
    LINK_OPTIONS "-Wl,--whole-archive ${CMAKE_CURRENT_BINARY_DIR}/lib${COMPONENT_NAME}.a -Wl,--no-whole-archive"
    APPEND)