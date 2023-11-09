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
    "rtc_slow_nif.c"
    "socket_driver.c"
    "spi_driver.c"
    "uart_driver.c"
    "otp_crypto_platform.c"
    "otp_net_platform.c"
    "otp_socket_platform.c"
    "otp_ssl_platform.c"
)

if (IDF_VERSION_MAJOR GREATER_EQUAL 5)
    set(ADDITIONAL_PRIV_REQUIRES "esp_hw_support" "efuse")
else()
    set(ADDITIONAL_PRIV_REQUIRES "")
endif()

if(CONFIG_AVM_ENABLE_OTP_SSL_NIFS OR CONFIG_AVM_ENABLE_OTP_CRYPTO_NIFS)
   set(ADDITIONAL_PRIV_REQUIRES ${ADDITIONAL_PRIV_REQUIRES} "mbedtls")
endif()

# WHOLE_ARCHIVE option is supported only with esp-idf 5.x
# A link option will be used with esp-idf 4.x
if (IDF_VERSION_MAJOR EQUAL 5)
    set(OPTIONAL_WHOLE_ARCHIVE WHOLE_ARCHIVE)
else()
    set(OPTIONAL_WHOLE_ARCHIVE "")
endif()

idf_component_register(
    SRCS ${AVM_BUILTIN_COMPONENT_SRCS}
    INCLUDE_DIRS "include"
    PRIV_REQUIRES "libatomvm" "avm_sys" "nvs_flash" "driver" "esp_event" "esp_wifi" ${ADDITIONAL_PRIV_REQUIRES}
    ${OPTIONAL_WHOLE_ARCHIVE}
)

if (IDF_VERSION_MAJOR EQUAL 4)
    idf_build_set_property(
        LINK_OPTIONS "-Wl,--whole-archive ${CMAKE_CURRENT_BINARY_DIR}/lib${COMPONENT_NAME}.a -Wl,--no-whole-archive"
        APPEND)
endif()

target_compile_features(${COMPONENT_LIB} INTERFACE c_std_11)
