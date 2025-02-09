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

project(gleam_avm)

include(BuildGleam)

set(GLEAM_MODULES
    gleam_avm/atomvm
)

pack_gleam_archive(gleam_avm ${GLEAM_MODULES})

include(../../version.cmake)

set(GLEAM_AVM_VERSION ${ATOMVM_BASE_VERSION})

install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/build/prod/erlang/gleam_avm/ebin/
    DESTINATION lib/atomvm/lib/gleam_avm-${GLEAM_AVM_VERSION}/ebin
    FILES_MATCHING PATTERN "*.beam"
)

install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/gleam_avm.avm
    DESTINATION lib/atomvm/lib/gleam_avm-${GLEAM_AVM_VERSION}/ebin/
)

install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/gleam_avm/src
    DESTINATION lib/atomvm/lib/gleam_avm-${GLEAM_AVM_VERSION}/src
    FILES_MATCHING PATTERN "*.gleam"
)
