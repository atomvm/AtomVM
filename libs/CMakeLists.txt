#
# This file is part of AtomVM.
#
# Copyright 2018 Fred Dushin <fred@dushin.net>
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

cmake_minimum_required (VERSION 3.12)
project(libs)

add_subdirectory(estdlib/src)
add_subdirectory(eavmlib/src)
add_subdirectory(alisp/src)
add_subdirectory(etest/src)
add_subdirectory(esp32boot)
add_subdirectory(esp32devmode/src)

if (Elixir_FOUND)
    add_subdirectory(exavmlib/lib)
else()
    message("Unable to find elixirc -- skipping Elixir libs")
endif()

if (Elixir_FOUND)
    pack_lib(atomvmlib eavmlib estdlib alisp exavmlib)
else()
    pack_lib(atomvmlib eavmlib estdlib alisp)
endif()

if (Dialyzer_FOUND)
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/atomvmlib.plt
        DEPENDS atomvmlib
        COMMAND dialyzer --build_plt --output_plt ${CMAKE_CURRENT_BINARY_DIR}/atomvmlib.plt
            -r ${CMAKE_CURRENT_BINARY_DIR}/estdlib/src/beams
            -r ${CMAKE_CURRENT_BINARY_DIR}/eavmlib/src/beams
            -r ${CMAKE_CURRENT_BINARY_DIR}/alisp/src/beams
    )
    add_custom_target(atomvmlib_plt
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/atomvmlib.plt
    )
else()
    message("Dialyzer was not found -- skipping PLT build")
endif()

install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/atomvmlib.avm
    DESTINATION lib/atomvm
)
