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

cmake_minimum_required (VERSION 3.13)
project (code_load)

include(BuildErlang)

compile_erlang(export_test_module)
jit_precompile(export_test_module)
generate_hrl(export_test_module_data.hrl EXPORT_TEST_MODULE_DATA export_test_module.beam)

pack_precompiled_archive(code_load_pack export_test_module)
generate_hrl(code_load_pack_data.hrl CODE_LOAD_PACK_DATA code_load_pack.avm)

set(code_load_deps
    export_test_module.beam
    export_test_module_data.hrl
    code_load_pack
    code_load_pack_data.hrl
)
if(NOT AVM_DISABLE_JIT)
    set(code_load_deps ${code_load_deps} jit)
endif()

add_custom_target(code_load_files DEPENDS ${code_load_deps})
