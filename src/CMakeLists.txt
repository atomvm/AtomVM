cmake_minimum_required (VERSION 2.6)
project (AtomVM)

add_subdirectory(libAtomVM)

if(CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pedantic -Wextra -ggdb")
endif()

include_directories(${CMAKE_CURRENT_BINARY_DIR} libAtomVM/)

add_executable(AtomVM main.c platforms/${CMAKE_SYSTEM_NAME}/sys.c platforms/${CMAKE_SYSTEM_NAME}/mapped_file.c platforms/${CMAKE_SYSTEM_NAME}/udpdriver.c)
target_link_libraries(AtomVM libAtomVM)
set_property(TARGET AtomVM PROPERTY C_STANDARD 99)

if (CMAKE_BUILD_TYPE STREQUAL "Coverage")
    set_target_properties(AtomVM PROPERTIES COMPILE_FLAGS "-O0 -fprofile-arcs -ftest-coverage")
endif()
