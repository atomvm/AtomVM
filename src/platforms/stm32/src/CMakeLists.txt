#
# This file is part of AtomVM.
#
# Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

# Specify output executable
set(PROJECT_EXECUTABLE ${PROJECT_NAME}-${DEVICE}.elf)
add_executable(${PROJECT_EXECUTABLE} main.c)

target_compile_features(${PROJECT_EXECUTABLE} PUBLIC c_std_11)
if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(${PROJECT_EXECUTABLE} PUBLIC -Wall -pedantic -Wextra -ggdb -std=gnu11)
endif()

add_subdirectory(../../../libAtomVM libAtomVM)
target_link_libraries(${PROJECT_EXECUTABLE} PUBLIC libAtomVM)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

add_subdirectory(lib)
target_link_libraries(${PROJECT_EXECUTABLE} PRIVATE libAtomVM${PLATFORM_LIB_SUFFIX})

# Output elf file size
add_custom_command(
    TARGET ${PROJECT_EXECUTABLE}
    POST_BUILD
    COMMAND ${ARM_SIZE} ${PROJECT_EXECUTABLE}
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
)
