#
# This file is part of AtomVM.
#
# Copyright 2019-2021 Riccardo Binetti <rbino@gmx.com>
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

project(exavmlib)

include(BuildElixir)

set(ELIXIR_MODULES
    AVMPort
    Bitwise
    Code
    Console
    GPIO
    I2C
    Integer
    LEDC
    Access
    Enum
    IO
    List
    Map
    Module
    Keyword
    Kernel
    Process
    Tuple
)

pack_archive(exavmlib ${ELIXIR_MODULES})

include(../../../version.cmake)

set(EXAVMLIB_VERSION ${ATOMVM_BASE_VERSION})

install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/beams/
    DESTINATION lib/atomvm/lib/exavmlib-${EXAVMLIB_VERSION}/ebin
    FILES_MATCHING PATTERN "*.beam"
)

install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/exavmlib.avm
    DESTINATION lib/atomvm/lib/exavmlib-${EXAVMLIB_VERSION}/ebin/
)

install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/
    DESTINATION lib/atomvm/lib/exavmlib-${EXAVMLIB_VERSION}/lib
    FILES_MATCHING PATTERN "*.ex"
)
