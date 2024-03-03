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

# GCC complains about "warning: #include_next is a GCC extension" when using pedantic flag
# "pedantic" flag should be disabled rather poluting diagnostics with warnings caused from 3rd party
option(AVM_PEDANTIC_WARNINGS "Pedantic compiler warnings" OFF)

add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/../../../../libAtomVM" "libAtomVM")

# Add directory with platform_atomic.h if we mean to use it
if (HAVE_PLATFORM_ATOMIC_H)
    target_include_directories(libAtomVM PUBLIC ../avm_sys/)
endif()

target_link_libraries(${COMPONENT_LIB}
    INTERFACE libAtomVM "-u platform_nifs_get_nif" "-u platform_defaultatoms_init")

target_compile_features(${COMPONENT_LIB} INTERFACE c_std_11)
