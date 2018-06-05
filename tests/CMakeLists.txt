cmake_minimum_required (VERSION 2.6)
project (tests)

add_subdirectory(erlang_tests)

if(CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pedantic -Wextra -ggdb")
endif()

include_directories(${CMAKE_CURRENT_BINARY_DIR} ../src/libAtomVM/)

add_executable(test-erlang test.c ../src/platforms/${CMAKE_SYSTEM_NAME}/sys.c ../src/platforms/${CMAKE_SYSTEM_NAME}/mapped_file.c)
target_link_libraries(test-erlang libAtomVM)
set_property(TARGET test-erlang PROPERTY C_STANDARD 99)
add_dependencies(test-erlang erlang_test_modules)

add_executable(test-structs test-structs.c ../src/platforms/${CMAKE_SYSTEM_NAME}/sys.c ../src/platforms/${CMAKE_SYSTEM_NAME}/mapped_file.c)
target_link_libraries(test-structs libAtomVM)
set_property(TARGET test-structs PROPERTY C_STANDARD 99)

if (CMAKE_BUILD_TYPE STREQUAL "Coverage")
    set (PROJECT_TEST_NAME test-erlang)
    set_target_properties(test-erlang PROPERTIES COMPILE_FLAGS "-O0 -fprofile-arcs -ftest-coverage")
endif()
