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

cmake_minimum_required (VERSION 3.13)
include($ENV{IDF_PATH}/tools/cmake/project.cmake)
project(atomvm-esp32)

idf_build_get_property(c_compile_options C_COMPILE_OPTIONS)
list(REMOVE_ITEM c_compile_options -std=gnu99)
list(APPEND c_compile_options -std=gnu11)
idf_build_set_property(C_COMPILE_OPTIONS ${c_compile_options})

# Options that make sense for this platform
option(AVM_DISABLE_SMP "Disable SMP." OFF)
option(AVM_USE_32BIT_FLOAT "Use 32 bit floats." OFF)
option(AVM_VERBOSE_ABORT "Print module and line number on VM abort" OFF)
option(AVM_CREATE_STACKTRACES "Create stacktraces" ON)

add_subdirectory(tools)
