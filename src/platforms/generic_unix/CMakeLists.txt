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

add_executable(AtomVM main.c)
set_target_properties(AtomVM PROPERTIES RUNTIME_OUTPUT_DIRECTORY ../../ ENABLE_EXPORTS ON)
target_compile_features(AtomVM PUBLIC c_std_11)
if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(AtomVM PUBLIC -Wall -pedantic -Wextra -ggdb)
elseif(CMAKE_C_COMPILER_ID MATCHES "Clang")
    target_compile_options(AtomVM PUBLIC -Wall -pedantic -Wextra -g)
endif()

if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    include(CheckLibraryExists)
    check_library_exists(rt clock_gettime "time.h" HAVE_CLOCK_GETTIME)
    if (HAVE_CLOCK_GETTIME)
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lrt")
        set(CMAKE_C_STANDARD_LIBRARIES "${CMAKE_C_STANDARD_LIBRARIES} -lrt")
    else()
        # might also be in libc
        check_library_exists(c clock_gettime "" HAVE_CLOCK_GETTIME)
    endif()
endif()


add_subdirectory(lib)
target_include_directories(AtomVM PUBLIC lib/)

add_subdirectory(../../libAtomVM libAtomVM)
target_link_libraries(AtomVM PRIVATE libAtomVM)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)
target_link_libraries(AtomVM PRIVATE libAtomVM${PLATFORM_LIB_SUFFIX})

if(AVM_DISABLE_JIT)
set(precompiled_suffix)
else()
set(precompiled_suffix "-${AVM_JIT_TARGET_ARCH}")
include(DefineIfExists)
define_if_function_exists(libAtomVM${PLATFORM_LIB_SUFFIX} pthread_jit_write_protect_np "pthread.h" PUBLIC HAVE_PTHREAD_JIT_WRITE_PROTECT_NP)
define_if_symbol_exists(libAtomVM${PLATFORM_LIB_SUFFIX} MAP_JIT "sys/mman.h" PUBLIC HAVE_MAP_JIT)
endif()

if (COVERAGE)
    include(CodeCoverage)
    append_coverage_compiler_flags_to_target(AtomVM)
    append_coverage_linker_flags_to_target(AtomVM)
endif()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/atomvm ${CMAKE_CURRENT_BINARY_DIR}/atomvm @ONLY)

install(TARGETS AtomVM DESTINATION lib/atomvm)
install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/atomvm
    DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)
