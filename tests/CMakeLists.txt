cmake_minimum_required (VERSION 2.6)
project (tests)

add_subdirectory(erlang_tests)

if(CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pedantic -Wextra -ggdb")
endif()

include_directories(${CMAKE_CURRENT_BINARY_DIR} ../src/libAtomVM/)

add_executable(test-erlang test.c)
target_link_libraries(test-erlang libAtomVM)
set_property(TARGET test-erlang PROPERTY C_STANDARD 99)
