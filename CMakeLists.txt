cmake_minimum_required (VERSION 2.6)

project(AtomVM)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/CMakeModules)

find_package(Elixir)

option(AVM_DISABLE_FP "Disable floating point support." OFF)
if (AVM_DISABLE_FP)
    add_definitions(-DAVM_NO_FP)
endif()

add_subdirectory(src)
add_subdirectory(tests)
add_subdirectory(tools/packbeam)
if (NOT "${CMAKE_GENERATOR}" MATCHES "Xcode")
    add_subdirectory(libs)
    add_subdirectory(examples)
    add_subdirectory(doc)
endif()

if (CMAKE_BUILD_TYPE STREQUAL "Coverage")
    include(CodeCoverage)
    SETUP_TARGET_FOR_COVERAGE(NAME coverage EXECUTABLE test-erlang DEPENDENCIES test-erlang)
endif()
