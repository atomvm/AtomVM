#
# This file is part of AtomVM.
#
# Copyright 2018 Riccardo Binetti <rbino@gmx.com>
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

# Enforce C/C++ standard level
set(CMAKE_C_STANDARD_REQUIRED YES)
set(CMAKE_CXX_STANDARD_REQUIRED YES)

# Enforce to disable any compiler-specific extensions
set(CMAKE_C_EXTENSIONS NO)
set(CMAKE_CXX_EXTENSIONS NO)

# Flags that apply to both C and C++
set(COMMON_WARN_FLAGS "${COMMON_WARN_FLAGS} -pedantic -Wall -Wextra")

# Flags that apply only to C OR C++
set(C_WARN_FLAGS "${COMMON_WARN_FLAGS}")
set(CXX_WARN_FLAGS "${COMMON_WARN_FLAGS}")

# Use C and C++ compiler optimizatons for size and speed.
if (${CMAKE_FLASH_SIZE} STREQUAL "ROM_512K")
set(OPTIMIZE_FLAG "-Os")
else()
set(OPTIMIZE_FLAG "-O2")
endif()

# Pass them back to the CMake variable
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${C_WARN_FLAGS} ${OPTIMIZE_FLAG}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CXX_WARN_FLAGS} ${OPTIMIZE_FLAG}")

if (LOG_VERBOSE)
    message("--------------Build Flags---------------")
    message(STATUS "C   : ${CMAKE_C_FLAGS}")
    message(STATUS "CXX : ${CMAKE_CXX_FLAGS}")
endif ()
