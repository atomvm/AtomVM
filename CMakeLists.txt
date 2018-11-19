cmake_minimum_required (VERSION 2.6)

project (AtomVM)

add_subdirectory(src)
add_subdirectory(tests)
add_subdirectory(tools/packbeam)

if (CMAKE_BUILD_TYPE STREQUAL "Coverage")
    set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/CMakeModules)
    include(CodeCoverage)
    SETUP_TARGET_FOR_COVERAGE(NAME coverage EXECUTABLE test-erlang DEPENDENCIES test-erlang)
endif()
