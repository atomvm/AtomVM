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
set(COMMON_WARN_FLAGS "${COMMON_WARN_FLAGS} -pedantic -Wall -Wextra \
-Wcast-align -Wcast-qual -Wconversion -Wdisabled-optimization -Wdouble-promotion -Wduplicated-cond \
-Wduplicated-branches -Wfloat-equal -Wformat=2 -Winit-self -Winline -Winvalid-pch -Wlogical-op \
-Wmissing-declarations -Wmissing-format-attribute -Wmissing-include-dirs -Wno-unused -Wnull-dereference -Wodr \
-Wpointer-arith -Wredundant-decls -Wrestrict -Wshadow -Wsign-conversion -Wstrict-overflow=5 -Wswitch-default \
-Wswitch-enum -Wwrite-strings -Wundef -Wuninitialized -Wunreachable-code")

# Flags that apply only to C OR C++
set(C_WARN_FLAGS "${COMMON_WARN_FLAGS} -Wbad-function-cast -Wmissing-prototypes -Wnested-externs")
set(CXX_WARN_FLAGS "${COMMON_WARN_FLAGS} -Wctor-dtor-privacy -Wnoexcept -Wold-style-cast -Woverloaded-virtual \
-Wsign-promo -Wstrict-null-sentinel -Wuseless-cast -Wzero-as-null-pointer-constant")

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
