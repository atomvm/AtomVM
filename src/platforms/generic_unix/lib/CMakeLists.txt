#
# This file is part of AtomVM.
#
# Copyright 2018-2020 Fred Dushin <fred@dushin.net>
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
project (libAtomVMPlatformGenericUnix)

set(HEADER_FILES
    otp_socket_platform.h
    generic_unix_sys.h
    mapped_file.h
    platform_defaultatoms.h
    ../../../libAtomVM/otp_net.h
    ../../../libAtomVM/otp_socket.h
)

set(SOURCE_FILES
    otp_socket_platform.c
    mapped_file.c
    platform_defaultatoms.c
    platform_nifs.c
    smp.c
    socket_driver.c
    sys.c
    ../../../libAtomVM/inet.c
    ../../../libAtomVM/otp_net.c
    ../../../libAtomVM/otp_socket.c
)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

add_library(libAtomVM${PLATFORM_LIB_SUFFIX} ${SOURCE_FILES} ${HEADER_FILES})
target_compile_features(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC c_std_11)
if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC -Wall -pedantic -Wextra -ggdb)
endif()

include(DefineIfExists)
define_if_function_exists(libAtomVM${PLATFORM_LIB_SUFFIX} signal "signal.h" PRIVATE HAVE_SIGNAL)
define_if_function_exists(libAtomVM${PLATFORM_LIB_SUFFIX} getservbyname "netdb.h" PRIVATE HAVE_SERVBYNAME)

target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC libAtomVM)
include_directories(${CMAKE_SOURCE_DIR}/src/platforms/generic_unix/lib)

include(MbedTLS)
if (MbedTLS_FOUND)
    target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC MbedTLS::mbedtls)
    target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ATOMVM_HAS_MBEDTLS)
    target_sources(libAtomVM${PLATFORM_LIB_SUFFIX}
        PRIVATE
        ../../../libAtomVM/otp_crypto.c
        ../../../libAtomVM/otp_crypto.h
        ../../../libAtomVM/otp_ssl.c
        ../../../libAtomVM/otp_ssl.h
    )
else()
    message("WARNING:  Could NOT find MbedTLS, ssl and crypto modules will not be supported. Install MbedTLS 3.x or try to set MBEDTLS_ROOT_DIR to installation prefix of MbedTLS 2.x")
endif()

# enable by default dynamic loading on unix
target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC DYNLOAD_PORT_DRIVERS)
target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ${CMAKE_DL_LIBS})

if (NOT AVM_DISABLE_SMP)
    set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
    find_package(Threads REQUIRED)
    target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ${CMAKE_THREAD_LIBS_INIT})
endif()

include(CheckSymbolExists)
check_symbol_exists(eventfd "sys/eventfd.h" HAVE_EVENTFD)
if (HAVE_EVENTFD)
    add_definitions(-DHAVE_EVENTFD)
endif()
check_symbol_exists(kqueue "sys/event.h" HAVE_KQUEUE)
check_symbol_exists(EVFILT_USER "sys/event.h" HAVE_EVFILT_USER)
check_symbol_exists(NOTE_TRIGGER "sys/event.h" HAVE_NOTE_TRIGGER)
if (HAVE_KQUEUE AND HAVE_EVFILT_USER AND HAVE_NOTE_TRIGGER)
    target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC HAVE_KQUEUE)
endif()
# pick one as an example
check_symbol_exists(EAI_BADHINTS "netdb.h" HAVE_EXTENDED_EAI_ERRNO)
if (HAVE_EXTENDED_EAI_ERRNO)
    target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC HAVE_EXTENDED_EAI_ERRNO)
endif()

if (COVERAGE)
    include(CodeCoverage)
    append_coverage_compiler_flags_to_target(libAtomVM${PLATFORM_LIB_SUFFIX})
endif()
