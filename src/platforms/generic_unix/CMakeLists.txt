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

cmake_minimum_required (VERSION 2.6)
project (libAtomVMPlatformGenericUnix)

include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_SOURCE_DIR}/src/libAtomVM)

set(HEADER_FILES
    generic_unix_sys.h
    mapped_file.h
    platform_defaultatoms.h
)

set(SOURCE_FILES
    sys.c
    mapped_file.c
    platform_defaultatoms.c
    platform_nifs.c
    socket_driver.c
)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

find_package(OpenSSL)
if (${OPENSSL_FOUND} STREQUAL TRUE)
    include_directories(${OPENSSL_INCLUDE_DIR})
    add_definitions(-DATOMVM_HAS_OPENSSL)
    set(EXTRA_LIBS ${OPENSSL_CRYPTO_LIBRARY})
else()
    message("WARNING:  Some crypto operations will not be supported.")
    set(EXTRA_LIBS "")
endif()

# enable by default dynamic loading on unix
add_definitions(-DDYNLOAD_PORT_DRIVERS)
set(EXTRA_LIBS ${EXTRA_LIBS} ${CMAKE_DL_LIBS})

if(CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pedantic -Wextra -ggdb")
endif()

add_library(libAtomVM${PLATFORM_LIB_SUFFIX} ${SOURCE_FILES} ${HEADER_FILES})
target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} libAtomVM ${EXTRA_LIBS})
set_property(TARGET libAtomVM${PLATFORM_LIB_SUFFIX} PROPERTY C_STANDARD 99)

install(
    FILES ${CMAKE_CURRENT_SOURCE_DIR}/atomvm
    DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

if (CMAKE_BUILD_TYPE STREQUAL "Coverage")
    set_target_properties(libAtomVM${PLATFORM_LIB_SUFFIX} PROPERTIES COMPILE_FLAGS "-O0 -fprofile-arcs -ftest-coverage")
endif()
