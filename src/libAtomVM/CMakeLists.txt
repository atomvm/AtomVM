#
# This file is part of AtomVM.
#
# Copyright 2017-2021 Davide Bettio <davide@uninstall.it>
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
project (libAtomVM)

set(HEADER_FILES
    atom.h
    atomshashtable.h
    avmpack.h
    bif.h
    bitstring.h
    context.h
    ccontext.h
    debug.h
    defaultatoms.h
    dictionary.h
    exportedfunction.h
    externalterm.h
    globalcontext.h
    iff.h
    interop.h
    list.h
    linkedlist.h
    mailbox.h
    memory.h
    module.h
    opcodes.h
    opcodesswitch.h
    overflow_helpers.h
    nifs.h
    platform_nifs.h
    port.h
    refc_binary.h
    scheduler.h
    smp.h
    synclist.h
    stacktrace.h
    sys.h
    term_typedef.h
    term.h
    timer_list.h
    trace.h
    utils.h
    valueshashtable.h
    ${CMAKE_CURRENT_BINARY_DIR}/version.h
)

set(SOURCE_FILES
    atom.c
    atomshashtable.c
    avmpack.c
    bif.c
    bitstring.c
    context.c
    debug.c
    defaultatoms.c
    dictionary.c
    externalterm.c
    globalcontext.c
    iff.c
    interop.c
    mailbox.c
    memory.c
    module.c
    nifs.c
    port.c
    refc_binary.c
    scheduler.c
    stacktrace.c
    term.c
    timer_list.c
    valueshashtable.c
)

add_library(libAtomVM ${SOURCE_FILES} ${HEADER_FILES})
target_include_directories(libAtomVM PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_features(libAtomVM PUBLIC c_std_11)
if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(libAtomVM PUBLIC -Wall -pedantic -Wextra -ggdb)
endif()

# AtomVM options used in libAtomVM
if (ADVANCED_TRACING)
    target_compile_definitions(libAtomVM PUBLIC ENABLE_ADVANCED_TRACE)
endif()

target_link_libraries(libAtomVM PUBLIC m)
include(CheckCSourceCompiles)
set(CMAKE_REQUIRED_FLAGS -Werror=unknown-pragmas)
check_c_source_compiles("
     #include <fenv.h>
     int main() {
         #pragma STDC FENV_ACCESS ON
        return 0;
     }
 " HAVE_PRAGMA_STDC_FENV_ACCESS)
if (HAVE_PRAGMA_STDC_FENV_ACCESS)
    target_compile_definitions(libAtomVM PUBLIC HAVE_PRAGMA_STDC_FENV_ACCESS)
endif()

if(${CMAKE_C_FLAGS} MATCHES -DAVM_NO_SMP)
    message("SMP is disabled by CFLAGS environment")
    set(AVM_DISABLE_SMP ON)
endif()

if (AVM_DISABLE_SMP)
    target_compile_definitions(libAtomVM PUBLIC AVM_NO_SMP)
else()
    include(CheckIncludeFile)
    CHECK_INCLUDE_FILE(stdatomic.h STDATOMIC_INCLUDE)
    if(HAVE_PLATFORM_SMP_H)
        target_compile_definitions(libAtomVM PUBLIC HAVE_PLATFORM_SMP_H)
    endif()
    include(CheckCSourceCompiles)
    check_c_source_compiles("
        #include <stdatomic.h>
        int main() {
            _Static_assert(ATOMIC_POINTER_LOCK_FREE == 2, \"Expected ATOMIC_POINTER_LOCK_FREE to be equal to 2\");
        }
    " ATOMIC_POINTER_LOCK_FREE_IS_TWO)
    if (NOT ATOMIC_POINTER_LOCK_FREE_IS_TWO AND NOT HAVE_PLATFORM_SMP_H)
        if (NOT STDATOMIC_INCLUDE)
            message(FATAL_ERROR "stdatomic.h cannot be found, you need to disable SMP on this platform or provide platform_smp.h and define HAVE_PLATFORM_SMP_H")
        else()
            message(FATAL_ERROR "Platform doesn't support atomic pointers, you need to disable SMP or provide platform_smp.h and define HAVE_PLATFORM_SMP_H")
        endif()
    endif()
endif()

if (AVM_USE_32BIT_FLOAT)
    target_compile_definitions(libAtomVM PUBLIC AVM_USE_SINGLE_PRECISION)
endif()

if (AVM_VERBOSE_ABORT)
    target_compile_definitions(libAtomVM PUBLIC AVM_VERBOSE_ABORT)
endif()

if(AVM_CREATE_STACKTRACES)
    target_compile_definitions(libAtomVM PUBLIC AVM_CREATE_STACKTRACES)
endif()

# Automatically use zlib if present to load .beam files
if (${CMAKE_SYSTEM_NAME} STREQUAL "Darwin" OR ${CMAKE_SYSTEM_NAME} STREQUAL "Linux" OR ${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")
    find_package(ZLIB)
    if (ZLIB_FOUND)
        target_compile_definitions(libAtomVM PRIVATE WITH_ZLIB)
        target_link_libraries(libAtomVM PUBLIC ${ZLIB_LIBRARIES})
    endif(ZLIB_FOUND)
endif()

function(gperf_generate input output)
    add_custom_command(
        OUTPUT ${output}
        COMMAND gperf -t ${input} > ${output}
        DEPENDS ${input}
        COMMENT "Hashing ${input}"
    )
endfunction()

gperf_generate(${CMAKE_CURRENT_SOURCE_DIR}/bifs.gperf bifs_hash.h)
gperf_generate(${CMAKE_CURRENT_SOURCE_DIR}/nifs.gperf nifs_hash.h)

add_custom_target(generated DEPENDS bifs_hash.h)
add_custom_target(generated-nifs-hash DEPENDS nifs_hash.h)

include(../../version.cmake)

if (ATOMVM_DEV)
    set(ATOMVM_GIT_REVISION "<unknown>")
    execute_process(
        COMMAND git rev-parse --short HEAD
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        OUTPUT_VARIABLE ATOMVM_GIT_REVISION
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    if (NOT ATOMVM_GIT_REVISION STREQUAL "")
        set(ATOMVM_VERSION "${ATOMVM_BASE_VERSION}+git.${ATOMVM_GIT_REVISION}")
    else()
        set(ATOMVM_VERSION ${ATOMVM_BASE_VERSION})
    endif()
else()
    set(ATOMVM_VERSION ${ATOMVM_BASE_VERSION})
endif()

# Add include to directory where version.h is generated so targets linking
# libAtomVM can access it
target_include_directories(libAtomVM PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/version.h.in ${CMAKE_CURRENT_BINARY_DIR}/version.h)

if (NOT ${CMAKE_SYSTEM_NAME} STREQUAL "Generic")
    target_link_libraries(libAtomVM PUBLIC libAtomVM${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR})
endif()

add_dependencies(libAtomVM generated generated-nifs-hash)

if (COVERAGE)
    include(CodeCoverage)
    append_coverage_compiler_flags_to_target(libAtomVM)
endif()
