#
# This file is part of AtomVM.
#
# Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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
project (PackBEAM)

set(PACKBEAM_PATH "" CACHE PATH "Path to PackBEAM source tree. If unset, hex package will be used")

if(NOT PACKBEAM_PATH STREQUAL "")
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/_checkouts)
    file(CREATE_LINK ${PACKBEAM_PATH} ${CMAKE_CURRENT_BINARY_DIR}/_checkouts/packbeam SYMBOLIC)
endif()

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/rebar.config
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_command(
    OUTPUT packbeam
    COMMAND rebar3 escriptize && cp _build/default/bin/packbeam .
    VERBATIM
)

add_custom_target(
    PackBEAM ALL
    DEPENDS packbeam
)

install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/packbeam
    DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)
