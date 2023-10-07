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

# Prepare souces in build directory
include(${CMAKE_SOURCE_DIR}/CMakeModules/GetVersion.cmake)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html/_static)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/edoc)
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/src DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/)
file(COPY ${CMAKE_SOURCE_DIR}/CONTRIBUTING.md DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/src)
file(COPY ${CMAKE_SOURCE_DIR}/CHANGELOG.md DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/src)
file(COPY ${CMAKE_SOURCE_DIR}/UPDATING.md DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/src)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/release-notes.md.in ${CMAKE_CURRENT_BINARY_DIR}/src/release-notes.md @ONLY)
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/edoc/edown_dep DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/edoc/)

# Configure libAtomVM restucturedtext skeleton.
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/src/apidocs/libatomvm/files)
file(GLOB SOURCE_FILES LIST_DIRECTORIES false RELATIVE ${CMAKE_SOURCE_DIR}/src/libAtomVM/ ${CMAKE_SOURCE_DIR}/src/libAtomVM/*.c ${CMAKE_SOURCE_DIR}/src/libAtomVM/*.h)
foreach(SOURCE_TARGET ${SOURCE_FILES})
    set(SOURCE_FILE ${SOURCE_TARGET})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/src/apidocs/libatomvm/file.rst.in ${CMAKE_CURRENT_BINARY_DIR}/src/apidocs/libatomvm/files/${SOURCE_FILE}.rst @ONLY)
endforeach(SOURCE_TARGET)

# Support for edoc -> markdown.
add_custom_target(edown-escript
    COMMAND rebar3 compile
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/edoc/edown_dep
    COMMENT "Preparing edown escript" VERBATIM
)

# Get the version tree name, tag if this is a tagged commit, otherwise master.
execute_process(COMMAND "bash" "-c" "tag=$(git for-each-ref --points-at=HEAD --format='%(refname:lstrip=2)' refs/tags); ( [ $tag ] && echo $tag ) || echo 'master'"
                OUTPUT_VARIABLE
                DOC_TREE_VERSION
                OUTPUT_STRIP_TRAILING_WHITESPACE )

##
## Erlang API documentation
##
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html/apidocs/erlang)
file(COPY ${CMAKE_SOURCE_DIR}/libs/include DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/edoc/)

foreach(ERLANG_LIB estdlib eavmlib alisp etest)
    set(ERL_LIB ${ERLANG_LIB}) # to allow passing ERL_LIB to gendoc.erl.in
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/edoc/gendoc.erl.in ${CMAKE_CURRENT_BINARY_DIR}/edoc/${ERLANG_LIB}/gendoc.erl @ONLY)
    file(COPY ${CMAKE_SOURCE_DIR}/libs/${ERLANG_LIB} DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/edoc/)
    add_custom_target(edoc-${ERLANG_LIB}
        COMMAND escript gendoc.erl ${ERLANG_LIB} src ${CMAKE_CURRENT_BINARY_DIR}/src/apidocs/erlang/${ERLANG_LIB}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/edoc/${ERLANG_LIB}
        COMMENT "Generating edoc markdown for ${ERLANG_LIB}" VERBATIM
        DEPENDS edown-escript
    )
    set(ERLANG_EDOC_TARGETS ${ERLANG_EDOC_TARGETS} edoc-${ERLANG_LIB})
endforeach(ERLANG_LIB)

# ##
# ## SVG files (from Graphviz dot files)
# ##
set(DOT_FILES
    globalcontext-processes
    globalcontext-atoms
    globalcontext-modules
)
find_package(Graphviz)
if(GRAPHVIZ_FOUND)
    message("Graphiz found")
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/src/_static)
    foreach(DOT_FILE ${DOT_FILES})
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/src/_static/${DOT_FILE}.svg
            COMMAND dot -Tsvg ${CMAKE_CURRENT_SOURCE_DIR}/graphviz/${DOT_FILE}.dot > ${CMAKE_CURRENT_BINARY_DIR}/src/_static/${DOT_FILE}.svg
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/graphviz/${DOT_FILE}.dot
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT "Generating SVG for ${DOT_FILE}.dot" VERBATIM
        )
        set(ERLANG_DOTFILE_TARGETS ${ERLANG_DOTFILE_TARGETS} ${CMAKE_CURRENT_BINARY_DIR}/src/_static/${DOT_FILE}.svg)
    endforeach()
else()
    message("WARNING: Graphviz not found.  Some images may be missing in generated documentation.")
endif()

##
## Sphinx documentation
##
find_package(Sphinx)
if(SPHINX_FOUND)

    message("Sphinx found: ${SPHINX_BUILD_EXECUTABLE}")
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/conf.py.in ${CMAKE_CURRENT_BINARY_DIR}/conf.py @ONLY)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html/apidocs/libatomvm)
    add_custom_target(sphinx-html
        ${SPHINX_BUILD_EXECUTABLE} -b html -c ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}/src/ ${CMAKE_CURRENT_BINARY_DIR}/html/
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating Sphinx HTML documentation" VERBATIM
        DEPENDS ${ERLANG_DOTFILE_TARGETS} ${ERLANG_EDOC_TARGETS}
    )

    add_custom_target(sphinx-pdf
        ${SPHINX_BUILD_EXECUTABLE} -D exclude_patterns=**/c_api_docs.rst,**/libatomvm/** -b rinoh -c ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}/src/ ${CMAKE_CURRENT_BINARY_DIR}/pdf/
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating Sphinx PDF documentation" VERBATIM
        DEPENDS ${ERLANG_DOTFILE_TARGETS} ${ERLANG_EDOC_TARGETS} ${CMAKE_CURRENT_BINARY_DIR}/conf.py
    )

    add_custom_target(sphinx-epub
        ${SPHINX_BUILD_EXECUTABLE} -D exclude_patterns=**/c_api_docs.rst,**/libatomvm/** -b epub -c ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}/src/ ${CMAKE_CURRENT_BINARY_DIR}/epub/
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating Sphinx EPub documentation" VERBATIM
        DEPENDS ${ERLANG_DOTFILE_TARGETS} ${ERLANG_EDOC_TARGETS} ${CMAKE_CURRENT_BINARY_DIR}/conf.py
    )

    ## This target is intended for CI `Publish Docs` workflow.
    if ($ENV{CI})
        add_custom_target(GitHub_CI_Publish_Docs
            COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_BINARY_DIR}/html /home/runner/work/AtomVM/AtomVM/www/doc/${DOC_TREE_VERSION}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT "Copying html docs to publish location /home/runner/work/AtomVM/AtomVM/www/doc/${DOC_TREE_VERSION}." VERBATIM
            DEPENDS doc /home/runner/work/AtomVM/AtomVM/www/doc $ENV{DOC_TREE_VERSION}
        )
    endif()

else()
    message("Unable to find Sphinx -- no Sphinx documentation will be generated")
endif()

## Fix URLs and change title to include "library" instead of "application"
foreach(LIBAVM_ERL_LIB estdlib eavmlib alisp etest)
    add_custom_command(TARGET edoc-${LIBAVM_ERL_LIB}
        POST_BUILD
        COMMAND sed -i -e "s/\.md/\.html/g; s/application/library/g" ${CMAKE_CURRENT_BINARY_DIR}/src/apidocs/erlang/${LIBAVM_ERL_LIB}/README.md
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Fixing html link locations for ${LIBAVM_ERL_LIB}." VERBATIM
    )
endforeach(LIBAVM_ERL_LIB)

add_custom_target(doc #ALL
    DEPENDS ${ERLANG_EDOC_TARGETS} sphinx-html sphinx-pdf sphinx-epub
)

if (TARGET GitHub_CI_Publish_Docs)

    add_custom_command(
        COMMAND mkdir -p /home/runner/work/AtomVM/AtomVM/www/doc
        OUTPUT /home/runner/work/AtomVM/AtomVM/www/doc
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Prepare publish directory structure." VERBATIM
    )

    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html/pdf)
    add_custom_command(TARGET sphinx-pdf POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/pdf/AtomVM-${DOC_TREE_VERSION}.pdf ${CMAKE_CURRENT_BINARY_DIR}/html/pdf/
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/pdf
        COMMENT "Copying pdf to download location" VERBATIM
        DEPENDS $ENV{DOC_TREE_VERSION}
    )

    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html/epub)
    add_custom_command(TARGET sphinx-epub POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/epub/AtomVM-${DOC_TREE_VERSION}.epub ${CMAKE_CURRENT_BINARY_DIR}/html/epub/
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/epub
        COMMENT "Copying epub to download location." VERBATIM
        DEPENDS $ENV{DOC_TREE_VERSION}
    )

    add_custom_command(TARGET sphinx-html POST_BUILD
        COMMAND touch .nojekyll
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html
        COMMENT "Creating .nojekyll to allow style content on GitHub Pages." VERBATIM
    )

endif()
