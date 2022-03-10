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

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html)

##
## Erlang API documentation
##
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html/apidocs/erlang)
foreach(ERLANG_LIB estdlib eavmlib alisp etest)
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html/apidocs/erlang/${ERLANG_LIB})
    add_custom_target(edoc-${ERLANG_LIB}
        COMMAND escript ${CMAKE_SOURCE_DIR}/tools/doc/gendoc.erl ${ERLANG_LIB} ${CMAKE_SOURCE_DIR}/libs/${ERLANG_LIB}/src ${CMAKE_CURRENT_BINARY_DIR}/html/apidocs/erlang/${ERLANG_LIB}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating edoc for ${ERLANG_LIB}"
        VERBATIM
    )
    set(ERLANG_EDOC_TARGETS ${ERLANG_EDOC_TARGETS} edoc-${ERLANG_LIB})
endforeach(ERLANG_LIB)

## TODO Uncomment when we incorporate Graphviz DOT files into documentation
# ##
# ## SVG files (from Graphviz dot files)
# ##
# set(DOT_FILES
#     globalcontext-processes
#     globalcontext-atoms
#     globalcontext-modules
# )
# find_package(Graphviz)
# if(GRAPHVIZ_FOUND)
#     message("Graphiz found")
#     file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html/svg)
#     foreach(DOT_FILE ${DOT_FILES})
#         add_custom_command(
#             OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/html/svg/${DOT_FILE}.svg
#             COMMAND dot -Tsvg ${CMAKE_CURRENT_SOURCE_DIR}/graphviz/${DOT_FILE}.dot > ${CMAKE_CURRENT_BINARY_DIR}/html/svg/${DOT_FILE}.svg
#             DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/graphviz/${DOT_FILE}.dot
#             WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
#             COMMENT "Generating SVG for ${DOT_FILE}.dot"
#             VERBATIM
#         )
#         set(ERLANG_DOTFILE_TARGETS ${ERLANG_DOTFILE_TARGETS} ${CMAKE_CURRENT_BINARY_DIR}/html/svg/${DOT_FILE}.svg)
#     endforeach()
# else()
#     message("WARNING: Graphviz not found.  Some images may be missing in generated documentation.")
# endif()

##
## Doxygen (C) API documentation
##
find_package(Doxygen)
if(DOXYGEN_FOUND)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html/apidocs/atomvmlib)
    add_custom_target(doxygen-html # ALL
        ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating documentation" VERBATIM
        DEPENDS ${ERLANG_EDOC_TARGETS} ${ERLANG_DOTFILE_TARGETS}
    )
else()
    message("Unable to find doxygen -- no documentation will be generated")
endif()

##
## Sphinx documentation
##
find_package(Sphinx)
if(SPHINX_FOUND)

    include(${CMAKE_SOURCE_DIR}/CMakeModules/GetVersion.cmake)

    message("Sphinx found: ${SPHINX_BUILD_EXECUTABLE}")
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/conf.py.in ${CMAKE_CURRENT_BINARY_DIR}/conf.py @ONLY)
    add_custom_target(sphinx-html
        ${SPHINX_BUILD_EXECUTABLE} -b html -c ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_BINARY_DIR}/html
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating Sphinx HTML documentation" VERBATIM
        DEPENDS ${ERLANG_DOTFILE_TARGETS}
    )

    add_custom_target(sphinx-pdf
        ${SPHINX_BUILD_EXECUTABLE} -b rinoh -c ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_BINARY_DIR}/html/pdf
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating Sphinx PDF documentation" VERBATIM
        DEPENDS ${ERLANG_DOTFILE_TARGETS} ${CMAKE_CURRENT_BINARY_DIR}/conf.py
    )

    add_custom_target(sphinx-epub
        ${SPHINX_BUILD_EXECUTABLE} -b epub -c ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_BINARY_DIR}/html/epub
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating Sphinx PDF documentation" VERBATIM
        DEPENDS ${ERLANG_DOTFILE_TARGETS}
    )
else()
    message("Unable to find Sphinx -- no Sphinx documentation will be generated")
endif()


add_custom_target(doc #ALL
    DEPENDS ${ERLANG_EDOC_TARGETS} doxygen-html sphinx-html sphinx-pdf sphinx-epub
)
