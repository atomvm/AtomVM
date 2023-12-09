#
# This file is part of AtomVM.
#
# Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

# Find MbedTLS
# Search for MbedTLS 2.x or 3.x and define libraries like MbedTLS 3.x does.

# This script is not called FindMbedTLS.cmake because it would conflict with
# installed MbedTLS 3.x

# If MBEDTLS_ROOT_DIR is set, no heuristic is applied.
# It must be set to the parent directory of include/mbedtls/version.h
# Libraries are at ${MBEDTLS_LIBRARIES_DIR} or, if unset, ${MBEDTLS_ROOT_DIR}/lib/

# If MBEDTLS_ROOT_DIR is not set, apply the following heuristic:
# Try to find mbedtls 3.x CMake package with find_package
# If it doesn't work, search for MBEDTLS_VERSION_NUMBER symbol as well as
# the three libraries we need with check_symbol_exists and find_library

option(AVM_STATIC_MBEDTLS "Static link Mbed-TLS." OFF)

if (MBEDTLS_ROOT_DIR)
    set(MbedTLS_FOUND TRUE)
    if (NOT MBEDTLS_LIBRARIES_DIR)
        set(MBEDTLS_LIBRARIES_DIR ${MBEDTLS_ROOT_DIR}/lib)
    endif()
    message(STATUS "Will use MbedTLS from ${MBEDTLS_ROOT_DIR} and ${MBEDTLS_LIBRARIES_DIR}")

    if (AVM_STATIC_MBEDTLS)
        message(FATAL_ERROR "AVM_STATIC_MBEDTLS not supported with MbedTLS root dir option")
    endif()

    add_library(MbedTLS::mbedcrypto SHARED IMPORTED)
    set_target_properties(MbedTLS::mbedcrypto PROPERTIES
      IMPORTED_LOCATION "${MBEDTLS_LIBRARIES_DIR}/libmbedcrypto${CMAKE_SHARED_LIBRARY_SUFFIX}"
      INTERFACE_INCLUDE_DIRECTORIES "${MBEDTLS_ROOT_DIR}/include/"
    )

    add_library(MbedTLS::mbedx509 SHARED IMPORTED)
    set_target_properties(MbedTLS::mbedx509 PROPERTIES
      IMPORTED_LOCATION "${MBEDTLS_LIBRARIES_DIR}/libmbedx509${CMAKE_SHARED_LIBRARY_SUFFIX}"
      INTERFACE_INCLUDE_DIRECTORIES "${MBEDTLS_ROOT_DIR}/include/"
      INTERFACE_LINK_LIBRARIES "MbedTLS::mbedcrypto"
    )

    add_library(MbedTLS::mbedtls SHARED IMPORTED)
    set_target_properties(MbedTLS::mbedtls PROPERTIES
      IMPORTED_LOCATION "${MBEDTLS_LIBRARIES_DIR}/libmbedtls${CMAKE_SHARED_LIBRARY_SUFFIX}"
      INTERFACE_INCLUDE_DIRECTORIES "${MBEDTLS_ROOT_DIR}/include/"
      INTERFACE_LINK_LIBRARIES "MbedTLS::mbedx509"
    )
else()
    # MbedTLS 3.x is installed as a CMake package
    find_package(MbedTLS QUIET)
    if (MbedTLS_FOUND)
        message(STATUS "Found MbedTLS package ${MbedTLS_FOUND}")

        if (AVM_STATIC_MBEDTLS)
            message(FATAL_ERROR "AVM_STATIC_MBEDTLS not supported with MbedTLS cmake package")
        endif()
    else()
        if (AVM_STATIC_MBEDTLS)
            set(MBEDCRYPTO_LIB_NAME "libmbedcrypto.a")
            set(MBEDX509_LIB "libmbedx509.a")
            set(MBEDTLS_LIB "libmbedtls.a")
        else()
            set(MBEDCRYPTO_LIB_NAME "mbedcrypto")
            set(MBEDX509_LIB "mbedx509")
            set(MBEDTLS_LIB "mbedtls")
        endif()

        include(CheckSymbolExists)
        check_symbol_exists(MBEDTLS_VERSION_NUMBER "mbedtls/version.h" HAVE_MBEDTLS_VERSION_NUMBER)
        find_library(MBEDCRYPTO NAMES ${MBEDCRYPTO_LIB_NAME})
        find_library(MBEDX509 NAMES ${MBEDX509_LIB})
        find_library(MBEDTLS NAMES ${MBEDTLS_LIB})
        if (HAVE_MBEDTLS_VERSION_NUMBER
            AND NOT ${MBEDCRYPTO} STREQUAL "MBEDCRYPTO-NOTFOUND"
            AND NOT ${MBEDX509} STREQUAL "MBEDX509-NOTFOUND"
            AND NOT ${MBEDTLS} STREQUAL "MBEDTLS-NOTFOUND")
            message(STATUS "Found MbedTLS with mbedcrypto ${MBEDCRYPTO}, mbedx509 ${MBEDX509} and mbedtls ${MBEDTLS}")
            set(MbedTLS_FOUND TRUE)
            add_library(MbedTLS::mbedcrypto SHARED IMPORTED)
            set_target_properties(MbedTLS::mbedcrypto PROPERTIES
              IMPORTED_LOCATION "${MBEDCRYPTO}"
            )

            add_library(MbedTLS::mbedx509 SHARED IMPORTED)
            set_target_properties(MbedTLS::mbedx509 PROPERTIES
              IMPORTED_LOCATION "${MBEDX509}"
              INTERFACE_LINK_LIBRARIES "MbedTLS::mbedcrypto"
            )

            add_library(MbedTLS::mbedtls SHARED IMPORTED)
            set_target_properties(MbedTLS::mbedtls PROPERTIES
              IMPORTED_LOCATION "${MBEDTLS}"
              INTERFACE_LINK_LIBRARIES "MbedTLS::mbedx509"
            )
        endif()
    endif()
endif()
