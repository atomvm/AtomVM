##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

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
