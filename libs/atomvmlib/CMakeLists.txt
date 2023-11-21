#
# This file is part of AtomVM.
#
# Copyright 2023 Davide Bettio <davide@uninstall.it>
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

project(atomvmlib)

include(BuildErlang)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/rebar.config.in ${CMAKE_CURRENT_SOURCE_DIR}/rebar.config @ONLY)

if(Elixir_FOUND)
    rebar3_packbeam(atomvmlib eavmlib estdlib exavmlib)
else()
    rebar3_packbeam(atomvmlib eavmlib estdlib)
endif()

install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/_build/default/lib/atomvmlib.avm
    DESTINATION lib/atomvm
)
