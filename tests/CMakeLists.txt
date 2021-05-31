cmake_minimum_required (VERSION 2.6)
project (tests)

if (NOT "${CMAKE_GENERATOR}" MATCHES "Xcode")
    add_subdirectory(erlang_tests)
    add_subdirectory(libs/estdlib)
    add_subdirectory(libs/eavmlib)
    add_subdirectory(libs/alisp)
endif()

if(CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pedantic -Wextra -ggdb")
endif()

include_directories(${CMAKE_CURRENT_BINARY_DIR} ../src/libAtomVM/)

if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    include(CheckFunctionExists)
    include(CheckLibraryExists)
    check_library_exists(rt clock_gettime "time.h" HAVE_CLOCK_GETTIME)
    if (HAVE_CLOCK_GETTIME)
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lrt")
        set(CMAKE_C_STANDARD_LIBRARIES "${CMAKE_C_STANDARD_LIBRARIES} -lrt")
    else()
        # might also be in libc
        check_library_exists(c clock_gettime "" HAVE_CLOCK_GETTIME)
    endif()
endif()

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

add_executable(test-erlang test.c)
target_link_libraries(test-erlang libAtomVM libAtomVM${PLATFORM_LIB_SUFFIX} libAtomVM)
set_property(TARGET test-erlang PROPERTY C_STANDARD 99)
if (NOT "${CMAKE_GENERATOR}" MATCHES "Xcode")
    add_dependencies(test-erlang erlang_test_modules)
endif()

add_executable(test-structs test-structs.c)
target_link_libraries(test-structs libAtomVM libAtomVM${PLATFORM_LIB_SUFFIX})
set_property(TARGET test-structs PROPERTY C_STANDARD 99)

if (CMAKE_BUILD_TYPE STREQUAL "Coverage")
    set (PROJECT_TEST_NAME test-erlang)
    set_target_properties(test-erlang PROPERTIES COMPILE_FLAGS "-O0 -fprofile-arcs -ftest-coverage")
endif()
