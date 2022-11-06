#
# This file is part of AtomVM.
#
# Copyright 2022 Davide Bettio <davide@uninstall.it>
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

idf_component_register(INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/../../../../libAtomVM")

if (CMAKE_SYSTEM_NAME STREQUAL "Generic")
    if (CONFIG_IDF_TARGET_ARCH_XTENSA)
        set(CMAKE_SYSTEM_NAME "Xtensa")
    elseif(CONFIG_IDF_TARGET_ARCH_RISCV)
        set(CMAKE_SYSTEM_NAME "RISC-V")
    endif()
endif()

if (CMAKE_SYSTEM_PROCESSOR STREQUAL "")
    if (CONFIG_IDF_TARGET)
        set(CMAKE_SYSTEM_PROCESSOR "${CONFIG_IDF_TARGET}")
    endif()
endif()

if (CMAKE_SYSTEM_VERSION STREQUAL "")
    if("${IDF_VERSION_MAJOR}.${IDF_VERSION_MINOR}" VERSION_LESS "4.1")
        set(SYSTEM_VERSION "<<unknown>>")
    elseif("${IDF_VERSION_MAJOR}.${IDF_VERSION_MINOR}" GREATER_EQUAL "4.1")
        execute_process(
            COMMAND idf.py --version
            WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
            OUTPUT_VARIABLE SYSTEM_VERSION
            OUTPUT_STRIP_TRAILING_WHITESPACE
        )
    endif()
    if (NOT SYSTEM_VERSION STREQUAL "")
        set(CMAKE_SYSTEM_VERSION "${SYSTEM_VERSION}")
    endif()
endif()

add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/../../../../libAtomVM" "libAtomVM")

target_link_libraries(${COMPONENT_LIB}
    INTERFACE libAtomVM "-u platform_nifs_get_nif" "-u platform_defaultatoms_init")

target_compile_features(${COMPONENT_LIB} INTERFACE c_std_11)
