cmake_minimum_required (VERSION 2.6)
project (tests)

add_subdirectory(erlang_tests)

if(CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pedantic -Wextra -ggdb")
endif()

include_directories(${CMAKE_CURRENT_BINARY_DIR} ../src/libAtomVM/)

if(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
    set(PLATFORM_DIR "generic_unix")
elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    set(PLATFORM_DIR "generic_unix")
elseif(${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")
    set(PLATFORM_DIR "generic_unix")
else()
    message(FATAL_ERROR "Unsupported platform: ${CMAKE_SYSTEM_NAME}")
endif()

add_executable(test-erlang test.c ../src/platforms/${PLATFORM_DIR}/sys.c ../src/platforms/${PLATFORM_DIR}/mapped_file.c ../src/platforms/${PLATFORM_DIR}/udpdriver.c)
target_link_libraries(test-erlang libAtomVM)
set_property(TARGET test-erlang PROPERTY C_STANDARD 99)
add_dependencies(test-erlang erlang_test_modules)

add_executable(test-structs test-structs.c ../src/platforms/${PLATFORM_DIR}/sys.c ../src/platforms/${PLATFORM_DIR}/mapped_file.c ../src/platforms/${PLATFORM_DIR}/udpdriver.c)
target_link_libraries(test-structs libAtomVM)
set_property(TARGET test-structs PROPERTY C_STANDARD 99)

if (CMAKE_BUILD_TYPE STREQUAL "Coverage")
    set (PROJECT_TEST_NAME test-erlang)
    set_target_properties(test-erlang PROPERTIES COMPILE_FLAGS "-O0 -fprofile-arcs -ftest-coverage")
endif()
