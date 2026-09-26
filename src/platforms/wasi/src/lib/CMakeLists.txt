#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
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
project (libAtomVMPlatformWASI)

# Disable mbedTLS unused targets that do not compile because wasi doesn't have
# the POSIX API they rely on
set(ENABLE_PROGRAMS OFF CACHE BOOL "" FORCE)
set(ENABLE_TESTING OFF CACHE BOOL "" FORCE)
include(FetchMbedTLS)

set(_mbedtls_wasi_cfg "MBEDTLS_USER_CONFIG_FILE=\"${CMAKE_CURRENT_SOURCE_DIR}/mbedtls_wasi_config.h\"")
foreach(_tgt mbedcrypto mbedtls mbedx509)
    if(TARGET ${_tgt})
        target_compile_definitions(${_tgt} PRIVATE "${_mbedtls_wasi_cfg}")
    endif()
endforeach()
unset(_mbedtls_wasi_cfg)
unset(_tgt)

set(HEADER_FILES
    wasi_sys.h
    "../../../../libAtomVM/otp_crypto.h"
)

set(SOURCE_FILES
    platform_defaultatoms.c
    platform_nifs.c
    smp.c
    sys.c
    "../../../../libAtomVM/otp_crypto.c"
)

if (WASI_TARGET MATCHES "wasip2")
    list(APPEND HEADER_FILES otp_socket_platform.h)
    list(APPEND SOURCE_FILES
        otp_socket_platform.c
        ../../../../libAtomVM/inet.c
        ../../../../libAtomVM/otp_net.c
        ../../../../libAtomVM/otp_socket.c
    )
endif()

if (NOT AVM_DISABLE_JIT)
    message(FATAL_ERROR "JIT is not supported on WASI: the WebAssembly sandbox prohibits executable memory allocation. Configure with -DAVM_DISABLE_JIT=ON.")
endif()

set(PLATFORM_LIB_SUFFIX ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR})

add_library(libAtomVM${PLATFORM_LIB_SUFFIX} ${SOURCE_FILES} ${HEADER_FILES})

target_compile_features(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC c_std_11)

target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC libAtomVM)

# Pull in the WASI compat shim (defined by the parent src/CMakeLists.txt).
target_compile_options(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE ${WASI_COMPAT_INCLUDE})

target_include_directories(libAtomVM${PLATFORM_LIB_SUFFIX}
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
)

target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ATOMVM_HAS_MBEDTLS)
target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC MbedTLS::mbedcrypto)

# otp_ssl needs the full mbedTLS (not just mbedcrypto) and is only useful when
# we have BSD sockets to wrap.
if (WASI_TARGET MATCHES "wasip2")
    target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC MbedTLS::mbedtls)
    target_sources(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE
        ../../../../libAtomVM/otp_ssl.c
        ../../../../libAtomVM/otp_ssl.h
    )
    include(DefineIfExists)
    define_if_function_exists(libAtomVM${PLATFORM_LIB_SUFFIX} gethostname "unistd.h" PRIVATE HAVE_GETHOSTNAME)
    define_if_function_exists(libAtomVM${PLATFORM_LIB_SUFFIX} getservbyname "netdb.h" PRIVATE HAVE_SERVBYNAME)
endif()

include(CheckCSourceCompiles)
get_target_property(_mbedcrypto_includes MbedTLS::mbedcrypto INTERFACE_INCLUDE_DIRECTORIES)
set(CMAKE_REQUIRED_INCLUDES ${_mbedcrypto_includes})
check_c_source_compiles("
#include <mbedtls/version.h>
#ifndef MBEDTLS_PSA_CRYPTO_C
#error PSA Crypto not available
#endif
int main(void) { return 0; }
" HAVE_PSA_CRYPTO)
unset(CMAKE_REQUIRED_INCLUDES)
unset(_mbedcrypto_includes)

if (HAVE_PSA_CRYPTO)
    target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC HAVE_PSA_CRYPTO)
endif()
