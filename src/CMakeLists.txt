cmake_minimum_required (VERSION 2.6)
project (AtomVM)

add_subdirectory(libAtomVM)

if(CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pedantic -Wextra -ggdb")
endif()

include_directories(${CMAKE_CURRENT_BINARY_DIR} libAtomVM/)

if(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
    set(PLATFORM_DIR "generic_unix")
elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    set(PLATFORM_DIR "generic_unix")
else()
    message(FATAL_ERROR "Unsupported platform: ${CMAKE_SYSTEM_NAME}")
endif()

add_executable(AtomVM main.c platforms/${PLATFORM_DIR}/sys.c platforms/${PLATFORM_DIR}/mapped_file.c platforms/${PLATFORM_DIR}/udpdriver.c)
target_link_libraries(AtomVM libAtomVM)
set_property(TARGET AtomVM PROPERTY C_STANDARD 99)

if (CMAKE_BUILD_TYPE STREQUAL "Coverage")
    set_target_properties(AtomVM PROPERTIES COMPILE_FLAGS "-O0 -fprofile-arcs -ftest-coverage")
endif()
