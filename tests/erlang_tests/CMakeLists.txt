#
# This file is part of AtomVM.
#
# Copyright 2017-2021 Davide Bettio <davide@uninstall.it>
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
project(erlang_tests)

include(BuildErlang)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/rebar.config.in ${CMAKE_CURRENT_SOURCE_DIR}/rebar.config @ONLY)

rebar3_packbeam(erlang_tests)

##
## generate headers for code loading tests
##

macro(generate_hrl in_file def_name out_file target_dep)
    add_custom_target(
        generate_hrl_${def_name} ALL
        COMMAND escript ${CMAKE_CURRENT_SOURCE_DIR}/tools/to_hrl.erl ${in_file} ${def_name} ${out_file}
        COMMENT "Generating ${out_file}"
        # DEPENDS ${in_file}
    )
    add_dependencies(
        generate_hrl_${def_name}
        ${target_dep}
    )
endmacro()

generate_hrl(
    ${CMAKE_CURRENT_BINARY_DIR}/../code_load/_build/default/lib/code_load/ebin/export_test_module.beam
    EXPORT_TEST_MODULE_DATA
    ${CMAKE_CURRENT_SOURCE_DIR}/include/export_test_module_data.hrl
    code_load_packbeam
)

generate_hrl(
    ${CMAKE_CURRENT_BINARY_DIR}/../code_load/_build/default/lib/code_load.avm
    CODE_LOAD_PACK_DATA
    ${CMAKE_CURRENT_SOURCE_DIR}/include/code_load_pack_data.hrl
    code_load_packbeam
)

add_custom_target(
    code_load_headers ALL
    COMMAND echo "Generating code_load_headers"
    DEPENDS
    generate_hrl_EXPORT_TEST_MODULE_DATA
    generate_hrl_CODE_LOAD_PACK_DATA
)

add_dependencies(erlang_tests_packbeam code_load_headers)

##
## copy the code_laod AVM into the ebin directory for code loading tests
##

add_dependencies(erlang_tests code_load)
add_custom_target(
    copy_code_load_avm ALL
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/_build/default/lib/erlang_tests/ebin/code_load && cp ${CMAKE_CURRENT_BINARY_DIR}/../code_load/code_load.avm ${CMAKE_CURRENT_BINARY_DIR}/_build/default/lib/erlang_tests/ebin/code_load/.
    COMMENT "Copying code_load.avm into erlang_tests ebin ..."
)
add_dependencies(copy_code_load_avm erlang_tests)
add_custom_target(
    copy_code_load_beam ALL
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/_build/default/lib/erlang_tests/ebin/code_load && cp ${CMAKE_CURRENT_BINARY_DIR}/../code_load/_build/default/lib/code_load/ebin/export_test_module.beam ${CMAKE_CURRENT_BINARY_DIR}/_build/default/lib/erlang_tests/ebin/code_load/.
    COMMENT "Copying code_load.avm into erlang_tests ebin ..."
)
add_dependencies(copy_code_load_beam erlang_tests)
