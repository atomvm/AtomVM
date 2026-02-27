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
project (UF2Tool)

set(UF2TOOL_PATH "" CACHE PATH "Path to UF2Tool source tree. If unset, hex package will be used")

if(NOT UF2TOOL_PATH STREQUAL "")
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/_checkouts)
    file(REMOVE ${CMAKE_CURRENT_BINARY_DIR}/_checkouts/uf2tool)
    execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ${UF2TOOL_PATH} ${CMAKE_CURRENT_BINARY_DIR}/_checkouts/uf2tool)
endif()

find_program(REBAR3 rebar3)
if(NOT REBAR3)
    message(FATAL_ERROR "rebar3 is required but was not found")
endif()

add_custom_command(
    OUTPUT uf2tool
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/rebar.config
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/rebar.config ${CMAKE_CURRENT_BINARY_DIR}/rebar.config
    COMMAND ${REBAR3} escriptize
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/_build/default/bin/uf2tool ${CMAKE_CURRENT_BINARY_DIR}/uf2tool
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    VERBATIM
)

add_custom_target(
    UF2Tool ALL
    DEPENDS uf2tool
)
