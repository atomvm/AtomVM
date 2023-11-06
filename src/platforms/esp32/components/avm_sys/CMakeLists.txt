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

cmake_minimum_required (VERSION 3.12)

set(AVM_SYS_COMPONENT_SRCS
    "smp.c"
    "sys.c"
    "platform_nifs.c"
    "platform_defaultatoms.c"
    "../../../../libAtomVM/inet.c"
    "../../../../libAtomVM/otp_crypto.c"
    "../../../../libAtomVM/otp_net.c"
    "../../../../libAtomVM/otp_socket.c"
    "../../../../libAtomVM/otp_ssl.c"
)

if (IDF_VERSION_MAJOR GREATER_EQUAL 5)
    set(ADDITIONAL_COMPONENTS "esp_partition")
    # available starting from v4.4
    set(ADDITIONAL_PRIV_REQUIRES "esp_hw_support")
else()
    set(ADDITIONAL_COMPONENTS "")
    set(ADDITIONAL_PRIV_REQUIRES "")
endif()

idf_component_register(
    SRCS ${AVM_SYS_COMPONENT_SRCS}
    INCLUDE_DIRS "include"
    REQUIRES "spi_flash" "soc" "newlib" "pthread" "vfs" "mbedtls" ${ADDITIONAL_COMPONENTS}
    PRIV_REQUIRES "libatomvm" "esp_timer" ${ADDITIONAL_PRIV_REQUIRES}
)

target_compile_features(${COMPONENT_LIB} INTERFACE c_std_11)

idf_component_get_property(soc_dir soc COMPONENT_DIR)
idf_component_get_property(soc_include_dirs soc INCLUDE_DIRS)
idf_build_get_property(idf_target IDF_TARGET)
idf_component_get_property(newlib_dir newlib COMPONENT_DIR)
idf_component_get_property(newlib_include_dirs newlib INCLUDE_DIRS)
idf_component_get_property(pthread_dir pthread COMPONENT_DIR)
idf_component_get_property(pthread_include_dirs pthread INCLUDE_DIRS)
idf_component_get_property(pthread_srcs pthread SRCS)

set(soc_target_include_dir ${soc_dir}/${target}/include)
list(TRANSFORM soc_include_dirs PREPEND ${soc_dir}/)
list(TRANSFORM newlib_include_dirs PREPEND ${newlib_dir}/)
list(TRANSFORM pthread_include_dirs PREPEND ${pthread_dir}/)

set(CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${soc_target_include_dir} ${soc_include_dirs} ${newlib_include_dirs} ${pthread_include_dirs} ${CMAKE_BINARY_DIR}/config/)

include(CheckSymbolExists)
include(CheckCSourceCompiles)

if ("${pthread_srcs}" MATCHES pthread_rwlock.c)
set(HAVE_PTHREAD_RWLOCK YES)
message(STATUS "pthread component includes pthread_rwlock.c, assuming it is available")
else()
check_c_source_compiles("
    #include <pthread.h>
    int main() {
        pthread_rwlock_t rwlock;
        pthread_rwlock_init(&rwlock, NULL);
    }
" HAVE_PTHREAD_RWLOCK)
endif()
if(HAVE_PTHREAD_RWLOCK)
    target_compile_definitions(${COMPONENT_LIB} INTERFACE HAVE_PTHREAD_RWLOCK)
endif()

check_c_source_compiles("
    #include \"soc/soc_caps.h\"
    int main() {
        return SOC_CPU_CORES_NUM;
    }
" HAVE_SOC_CPU_CORES_NUM)
if(HAVE_SOC_CPU_CORES_NUM)
    target_compile_definitions(${COMPONENT_LIB} INTERFACE HAVE_SOC_CPU_CORES_NUM)
endif()
