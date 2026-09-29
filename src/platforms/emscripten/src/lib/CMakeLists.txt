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

cmake_minimum_required (VERSION 3.13)
project (libAtomVMPlatformEmscripten)
include(FetchMbedTLS)

set(HEADER_FILES
    emscripten_sys.h
    websocket_nifs.h
    "../../../../libAtomVM/otp_crypto.h"
)

set(SOURCE_FILES
    platform_defaultatoms.c
    platform_nifs.c
    smp.c
    sys.c
    websocket_nifs.c
    "../../../../libAtomVM/otp_crypto.c"
)

if (NOT AVM_DISABLE_JIT)
    list(APPEND SOURCE_FILES jit_stream_wasm.c)
    list(APPEND HEADER_FILES jit_stream_wasm.h)
endif()

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

add_library(libAtomVM${PLATFORM_LIB_SUFFIX} ${SOURCE_FILES} ${HEADER_FILES})
target_compile_features(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC c_std_11)

target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC libAtomVM)
target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ATOMVM_HAS_MBEDTLS)
target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC MbedTLS::mbedtls)
include(CheckCSourceCompiles)
get_target_property(_mbedtls_includes MbedTLS::mbedtls INTERFACE_INCLUDE_DIRECTORIES)
set(CMAKE_REQUIRED_INCLUDES ${_mbedtls_includes})
set(_avm_try_compile_target_type ${CMAKE_TRY_COMPILE_TARGET_TYPE})
set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)
check_c_source_compiles("
    #include <mbedtls/version.h>
    #ifndef MBEDTLS_PSA_CRYPTO_C
    #error PSA Crypto not available
    #endif
    #include <psa/crypto.h>
    int main(void) { return 0; }
" HAVE_PSA_CRYPTO)
set(CMAKE_TRY_COMPILE_TARGET_TYPE ${_avm_try_compile_target_type})
unset(CMAKE_REQUIRED_INCLUDES)
unset(_avm_try_compile_target_type)
unset(_mbedtls_includes)
if (HAVE_PSA_CRYPTO)
    target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC HAVE_PSA_CRYPTO)
endif()
