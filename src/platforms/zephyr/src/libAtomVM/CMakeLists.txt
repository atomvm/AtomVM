#
# This file is part of AtomVM.
#
# Copyright 2020 Davide Bettio <davide@uninstall.it>
# Copyright 2020 Fred Dushin <fred@dushin.net>
# Copyright 2023 Adolfo E. García
# Copyright 2023 Winford (Uncle Grumpy) <winford@object.stream>
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

cmake_minimum_required(VERSION 3.20)
zephyr_library_named(libAtomVM)
project(libAtomVM)

function(gperf_generate input output)
    add_custom_command(
        OUTPUT ${output}
        COMMAND gperf -t ${input} > ${output}
        DEPENDS ${input}
        COMMENT "Hashing ${input}"
    )
endfunction()

set(ATOMVM_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../../../../)

gperf_generate(${ATOMVM_DIR}/src/libAtomVM/bifs.gperf bifs_hash.h)
gperf_generate(${ATOMVM_DIR}/src/libAtomVM/nifs.gperf nifs_hash.h)

add_custom_target(generated DEPENDS bifs_hash.h)
add_custom_target(generated-nifs-hash DEPENDS nifs_hash.h)

include(${ATOMVM_DIR}/version.cmake)

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

configure_file(${ATOMVM_DIR}/src/libAtomVM/version.h.in ${CMAKE_CURRENT_BINARY_DIR}/avm_version.h)
add_custom_target(version DEPENDS avm_version.h)

target_include_directories(libAtomVM PRIVATE
    ${CMAKE_CURRENT_BINARY_DIR}
    ${ATOMVM_DIR}/src/libAtomVM/
)

set(SOURCE_FILES
    ${ATOMVM_DIR}/src/libAtomVM/atom.c
    ${ATOMVM_DIR}/src/libAtomVM/atomshashtable.c
    ${ATOMVM_DIR}/src/libAtomVM/avmpack.c
    ${ATOMVM_DIR}/src/libAtomVM/bif.c
    ${ATOMVM_DIR}/src/libAtomVM/bitstring.c
    ${ATOMVM_DIR}/src/libAtomVM/context.c
    ${ATOMVM_DIR}/src/libAtomVM/debug.c
    ${ATOMVM_DIR}/src/libAtomVM/defaultatoms.c
    ${ATOMVM_DIR}/src/libAtomVM/dictionary.c
    ${ATOMVM_DIR}/src/libAtomVM/externalterm.c
    ${ATOMVM_DIR}/src/libAtomVM/globalcontext.c
    ${ATOMVM_DIR}/src/libAtomVM/iff.c
    ${ATOMVM_DIR}/src/libAtomVM/interop.c
    ${ATOMVM_DIR}/src/libAtomVM/mailbox.c
    ${ATOMVM_DIR}/src/libAtomVM/memory.c
    ${ATOMVM_DIR}/src/libAtomVM/module.c
    ${ATOMVM_DIR}/src/libAtomVM/nifs.c
    ${ATOMVM_DIR}/src/libAtomVM/port.c
    ${ATOMVM_DIR}/src/libAtomVM/posix_nifs.c
    ${ATOMVM_DIR}/src/libAtomVM/refc_binary.c
    ${ATOMVM_DIR}/src/libAtomVM/resources.c
    ${ATOMVM_DIR}/src/libAtomVM/scheduler.c
    ${ATOMVM_DIR}/src/libAtomVM/stacktrace.c
    ${ATOMVM_DIR}/src/libAtomVM/term.c
    ${ATOMVM_DIR}/src/libAtomVM/timer_list.c
    ${ATOMVM_DIR}/src/libAtomVM/valueshashtable.c
)

target_sources(libAtomVM PRIVATE ${SOURCE_FILES})

add_dependencies(libAtomVM generated generated-nifs-hash version) 

if (CONFIG_ATOMVM_ADVANCED_TRACING)
    zephyr_library_compile_definitions(ENABLE_ADVANCED_TRACE)
endif()

if (CONFIG_ATOMVM_DISABLE_SMP)
    zephyr_library_compile_definitions(AVM_NO_SMP)
    add_compile_definitions(AVM_NO_SMP)
else()
    include(CheckIncludeFile)
    CHECK_INCLUDE_FILE(stdatomic.h STDATOMIC_INCLUDE)
    if (HAVE_PLATFORM_SMP_H)
        zephyr_library_compile_definitions(HAVE_PLATFORM_SMP_H)
    endif()
    include(CheckCSourceCompiles)
    check_c_source_compiles("
        #include <stdatomic.h>
        int main() {
            _Static_assert(ATOMIC_POINTER_LOCK_FREE == 2, \"Expected ATOMIC_POINTER_LOCK_FREE to be equal to 2\");
        }
    " ATOMIC_POINTER_LOCK_FREE_IS_TWO)
    if(NOT ATOMIC_POINTER_LOCK_FREE_IS_TWO AND NOT HAVE_PLATFORM_SMP_H)
        if(NOT STDATOMIC_INCLUDE)
            message(FATAL_ERROR "stdatomic.h cannot be found, you need to disable SMP on this platform or provide platform_smp.h and define HAVE_PLATFORM_SMP_H")
        else()
            message(FATAL_ERROR "Platform doesn't support atomic pointers, you need to disable SMP or provide platform_smp.h and define HAVE_PLATFORM_SMP_H")
        endif()
    endif()
endif()

if (CONFIG_ATOMVM_USE_32BIT_FLOAT)
    zephyr_library_compile_definitions(AVM_USE_SINGLE_PRECISION)
endif()

if (CONFIG_ATOMVM_VERBOSE_ABORT)
    zephyr_library_compile_definitions(AVM_VERBOSE_ABORT)
endif()

if (CONFIG_ATOMVM_CREATE_STACKTRACES)
    zephyr_library_compile_definitions(AVM_CREATE_STACKTRACES)
endif()

include(DefineIfExists)
# HAVE_OPEN & HAVE_CLOSE are used in globalcontext.h
define_if_function_exists(libAtomVM open "zephyr/posix/fcntl.h" PUBLIC HAVE_OPEN)
define_if_function_exists(libAtomVM close "zephyr/posix/unistd.h" PUBLIC HAVE_CLOSE)
define_if_function_exists(libAtomVM mkfifo "zephyr/posix/sys/stat.h" PRIVATE HAVE_MKFIFO)
define_if_function_exists(libAtomVM unlink "zephyr/posix/unistd.h" PRIVATE HAVE_UNLINK)
define_if_symbol_exists(libAtomVM O_CLOEXEC "zephyr/posix/fcntl.h" PRIVATE HAVE_O_CLOEXEC)
define_if_symbol_exists(libAtomVM O_DIRECTORY "zephyr/posix/fcntl.h" PRIVATE HAVE_O_DIRECTORY)
define_if_symbol_exists(libAtomVM O_DSYNC "zephyr/posix/fcntl.h" PRIVATE HAVE_O_DSYNC)
define_if_symbol_exists(libAtomVM O_EXEC "zephyr/posix/fcntl.h" PRIVATE HAVE_O_EXEC)
define_if_symbol_exists(libAtomVM O_NOFOLLOW "zephyr/posix/fcntl.h" PRIVATE HAVE_O_NOFOLLOW)
define_if_symbol_exists(libAtomVM O_RSYNC "zephyr/posix/fcntl.h" PRIVATE HAVE_O_RSYNC)
define_if_symbol_exists(libAtomVM O_SEARCH "zephyr/posix/fcntl.h" PRIVATE HAVE_O_SEARCH)
define_if_symbol_exists(libAtomVM O_TTY_INIT "zephyr/posix/fcntl.h" PRIVATE HAVE_O_TTY_INIT)
define_if_symbol_exists(libAtomVM clock_settime "zephyr/posix/time.h" PRIVATE HAVE_CLOCK_SETTIME)
define_if_symbol_exists(libAtomVM settimeofday "zephyr/posix/sys/time.h" PRIVATE HAVE_SETTIMEOFDAY)
define_if_symbol_exists(libAtomVM socket "zephyr/posix/sys/socket.h" PUBLIC HAVE_SOCKET)
define_if_symbol_exists(libAtomVM select "zephyr/posix/sys/select.h" PUBLIC HAVE_SELECT)

if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(libAtomVM PUBLIC -Wall -pedantic -Wextra -ggdb)
endif()

set_property(TARGET libAtomVM PROPERTY C_STANDARD 11)

if (COVERAGE)
    include(CodeCoverage)
    append_coverage_compiler_flags_to_target(libAtomVM)
endif()
