#
# This file is part of AtomVM.
#
# Copyright 2018-2020 Fred Dushin <fred@dushin.net>
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

project(doc)

find_package(Doxygen)

if(DOXYGEN_FOUND)

    foreach(ERLANG_LIB estdlib eavmlib etest)
        file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html/ref/${ERLANG_LIB})
        add_custom_target(edoc-${ERLANG_LIB}
            COMMAND escript ${CMAKE_SOURCE_DIR}/tools/doc/gendoc.erl ${ERLANG_LIB} ${CMAKE_SOURCE_DIR}/libs/${ERLANG_LIB}/src ${CMAKE_CURRENT_BINARY_DIR}/html/ref/${ERLANG_LIB}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT "Generating edoc for ${ERLANG_LIB}"
            VERBATIM
        )
        set(ERLANG_EDOC_TARGETS ${ERLANG_EDOC_TARGETS} edoc-${ERLANG_LIB})
    endforeach(ERLANG_LIB)

    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
    add_custom_target(doc # ALL
        ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating documentation" VERBATIM
        DEPENDS ${ERLANG_EDOC_TARGETS}
    )
else()
    message("Unable to find doxygen -- no documentation will be generated")
endif()
