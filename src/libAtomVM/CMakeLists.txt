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
    atom_table.h
    avmpack.h
    bif.h
    bitstring.h
    context.h
    debug.h
    defaultatoms.h
    dictionary.h
    erl_nif.h
    erl_nif_priv.h
    exportedfunction.h
    externalterm.h
    globalcontext.h
    iff.h
    interop.h
    list.h
    listeners.h
    mailbox.h
    memory.h
    module.h
    opcodes.h
    opcodesswitch.h
    overflow_helpers.h
    nifs.h
    platform_nifs.h
    port.h
    posix_nifs.h
    refc_binary.h
    resources.h
    scheduler.h
    smp.h
    synclist.h
    stacktrace.h
    sys.h
    term_typedef.h
    term.h
    timer_list.h
    trace.h
    unicode.h
    utils.h
    valueshashtable.h
    ${CMAKE_CURRENT_BINARY_DIR}/avm_version.h
)

set(SOURCE_FILES
    atom.c
    atomshashtable.c
    atom_table.c
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
    posix_nifs.c
    refc_binary.c
    resources.c
    scheduler.c
    stacktrace.c
    term.c
    timer_list.c
    unicode.c
    valueshashtable.c
)

option(AVM_PEDANTIC_WARNINGS "Pedantic compiler warnings" ON)
if (AVM_PEDANTIC_WARNINGS)
    set(MAYBE_PEDANTIC_FLAG "-pedantic")
endif()

option(AVM_WARNINGS_ARE_ERRORS "Error on warning" OFF)
if (AVM_WARNINGS_ARE_ERRORS)
    set(MAYBE_WERROR_FLAG "-Werror")
endif()

add_library(libAtomVM ${SOURCE_FILES} ${HEADER_FILES})
target_include_directories(libAtomVM PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_features(libAtomVM PUBLIC c_std_11)
if (CMAKE_C_COMPILER_ID STREQUAL "GNU")
    target_compile_options(libAtomVM PUBLIC -Wall ${MAYBE_PEDANTIC_FLAG} ${MAYBE_WERROR_FLAG} -Wextra -ggdb -Werror=incompatible-pointer-types)
elseif (CMAKE_C_COMPILER_ID MATCHES "Clang")
    target_compile_options(libAtomVM PUBLIC -Wall --extra-warnings -Werror=incompatible-pointer-types ${MAYBE_WERROR_FLAG})
endif()

# AtomVM options used in libAtomVM
if (ADVANCED_TRACING)
    target_compile_definitions(libAtomVM PUBLIC ENABLE_ADVANCED_TRACE)
endif()

target_link_libraries(libAtomVM PUBLIC m)
include(CheckCSourceCompiles)
set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS} -Werror=unknown-pragmas")
check_c_source_compiles("
     #include <fenv.h>
     int main() {
         #pragma STDC FENV_ACCESS ON
        return 0;
     }
 " HAVE_PRAGMA_STDC_FENV_ACCESS FAIL_REGEX FENV_ACCESS)
if (HAVE_PRAGMA_STDC_FENV_ACCESS)
    target_compile_definitions(libAtomVM PUBLIC HAVE_PRAGMA_STDC_FENV_ACCESS)
endif()

if(${CMAKE_C_FLAGS} MATCHES -DAVM_NO_SMP)
    message("SMP is disabled by CFLAGS environment")
    set(AVM_DISABLE_SMP ON)
endif()

if (AVM_DISABLE_SMP)
    target_compile_definitions(libAtomVM PUBLIC AVM_NO_SMP)
endif()
if (NOT AVM_DISABLE_TASK_DRIVER)
    target_compile_definitions(libAtomVM PUBLIC AVM_TASK_DRIVER_ENABLED)
endif()

if(HAVE_PLATFORM_SMP_H)
    target_compile_definitions(libAtomVM PUBLIC HAVE_PLATFORM_SMP_H)
endif()
if(HAVE_PLATFORM_ATOMIC_H)
    target_compile_definitions(libAtomVM PUBLIC HAVE_PLATFORM_ATOMIC_H)
endif()

include(CheckIncludeFile)
CHECK_INCLUDE_FILE(stdatomic.h STDATOMIC_INCLUDE)
include(CheckCSourceCompiles)
check_c_source_compiles("
    #include <stdatomic.h>
    int main() {
        _Static_assert(ATOMIC_POINTER_LOCK_FREE == 2, \"Expected ATOMIC_POINTER_LOCK_FREE to be equal to 2\");
    }
" ATOMIC_POINTER_LOCK_FREE_IS_TWO)
if (ATOMIC_POINTER_LOCK_FREE_IS_TWO)
    target_compile_definitions(libAtomVM PUBLIC HAVE_ATOMIC)
endif()
if (NOT ATOMIC_POINTER_LOCK_FREE_IS_TWO AND NOT (HAVE_PLATFORM_ATOMIC_H OR (AVM_DISABLE_SMP AND AVM_DISABLE_TASK_DRIVER)))
    if (NOT STDATOMIC_INCLUDE)
        message(FATAL_ERROR "stdatomic.h cannot be found, you need to provide platform_atomic.h and define HAVE_PLATFORM_ATOMIC_H or alternatively pass AVM_DISABLE_SMP and AVM_DISABLE_TASK_DRIVER")
    else()
        message(FATAL_ERROR "Platform doesn't support atomic pointers, you need to provide platform_atomic.h and define HAVE_PLATFORM_ATOMIC_H or alternatively pass AVM_DISABLE_SMP and AVM_DISABLE_TASK_DRIVER")
    endif()
endif()

include(DefineIfExists)
# HAVE_OPEN & HAVE_CLOSE are used in globalcontext.h
define_if_function_exists(libAtomVM open "fcntl.h" PUBLIC HAVE_OPEN)
define_if_function_exists(libAtomVM close "unistd.h" PUBLIC HAVE_CLOSE)
define_if_function_exists(libAtomVM mkfifo "sys/stat.h" PRIVATE HAVE_MKFIFO)
define_if_function_exists(libAtomVM unlink "unistd.h" PRIVATE HAVE_UNLINK)
define_if_symbol_exists(libAtomVM O_CLOEXEC "fcntl.h" PRIVATE HAVE_O_CLOEXEC)
define_if_symbol_exists(libAtomVM O_DIRECTORY "fcntl.h" PRIVATE HAVE_O_DIRECTORY)
define_if_symbol_exists(libAtomVM O_DSYNC "fcntl.h" PRIVATE HAVE_O_DSYNC)
define_if_symbol_exists(libAtomVM O_EXEC "fcntl.h" PRIVATE HAVE_O_EXEC)
define_if_symbol_exists(libAtomVM O_NOFOLLOW "fcntl.h" PRIVATE HAVE_O_NOFOLLOW)
define_if_symbol_exists(libAtomVM O_RSYNC "fcntl.h" PRIVATE HAVE_O_RSYNC)
define_if_symbol_exists(libAtomVM O_SEARCH "fcntl.h" PRIVATE HAVE_O_SEARCH)
define_if_symbol_exists(libAtomVM O_TTY_INIT "fcntl.h" PRIVATE HAVE_O_TTY_INIT)
define_if_symbol_exists(libAtomVM clock_settime "time.h" PRIVATE HAVE_CLOCK_SETTIME)
define_if_symbol_exists(libAtomVM settimeofday "sys/time.h" PRIVATE HAVE_SETTIMEOFDAY)
define_if_symbol_exists(libAtomVM socket "sys/socket.h" PUBLIC HAVE_SOCKET)
define_if_symbol_exists(libAtomVM select "sys/select.h" PUBLIC HAVE_SELECT)

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

# Add include to directory where avm_version.h is generated so targets linking
# libAtomVM can access it
target_include_directories(libAtomVM PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/version.h.in ${CMAKE_CURRENT_BINARY_DIR}/avm_version.h)

if (NOT ${CMAKE_SYSTEM_NAME} STREQUAL "Generic")
    target_link_libraries(libAtomVM PUBLIC libAtomVM${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR})
endif()

add_dependencies(libAtomVM generated generated-nifs-hash)

if (COVERAGE)
    include(CodeCoverage)
    append_coverage_compiler_flags_to_target(libAtomVM)
endif()
