#
# This file is part of AtomVM.
#
# Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

ExternalProject_Add(HostAtomVM
    SOURCE_DIR ../../../../../../
    INSTALL_COMMAND cmake -E echo "Skipping install step."
    BUILD_COMMAND cmake --build . --target=atomvmlib
)

macro(rebar3_uf2create project_name)

    add_custom_target(
        ${project_name}_link_crypto ALL
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src
        COMMAND ln -f -s ${CMAKE_SOURCE_DIR}/../esp32/test/main/test_erl_sources/test_crypto.erl
        DEPENDS ../../../esp32/test/main/test_erl_sources/test_crypto.erl
        COMMENT "linking test_crypto.erl ..."
        VERBATIM
    )

    add_custom_target(
        ${project_name}_packbeam ALL
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMAND rebar3 atomvm packbeam -e ${CMAKE_CURRENT_BINARY_DIR}/HostAtomVM-prefix/src/HostAtomVM-build/libs/atomvmlib/atomvmlib.avm
        COMMAND_EXPAND_LISTS
        DEPENDS HostAtomVM ${project_name}_link_crypto
        COMMENT "Creating ${project_name} packbeam ..."
        VERBATIM
    )

    ## TODO the uf2create task might want to take an external AVM as input, as well as packbeam
    ## Alternatively, we might be ablet to do this via rebar.config using the atomvm_rebar2_plugin
    ## properties.  For now, we will just use Paul's uftool script

    # add_custom_target(
    # ${project_name}_uf2create ALL
    # WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    # COMMAND rebar3 atomvm uf2create
    # COMMAND_EXPAND_LISTS
    # DEPENDS ${project_name}_packbeam
    # COMMENT "Creating ${project_name} packbeam ..."
    # VERBATIM
    # )
    # add_custom_target(
    #     ${project_name} ALL
    #     WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    #     COMMAND ln -f -s _build/default/lib/${project_name}.uf2
    #     DEPENDS ${project_name}_uf2create
    #     VERBATIM
    # )

    add_custom_command(
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/rp2040_test_modules.uf2"
        COMMAND HostAtomVM-prefix/src/HostAtomVM-build/tools/uf2tool/uf2tool
            create
            -o rp2040_test_modules.uf2
            -s 0x10100000 "${CMAKE_CURRENT_BINARY_DIR}/_build/default/lib/rp2040_test_modules.avm"
        DEPENDS
            ${project_name}_packbeam
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        VERBATIM
    )

    add_custom_target(
        ${project_name} ALL
        DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/rp2040_test_modules.uf2"
    )

endmacro()

set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/rp2040_test_modules.uf2")

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/rebar.config.in ${CMAKE_CURRENT_SOURCE_DIR}/rebar.config @ONLY)
rebar3_uf2create(rp2040_test_modules)
add_dependencies(rp2040_test_modules HostAtomVM)
