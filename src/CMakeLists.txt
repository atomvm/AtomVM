cmake_minimum_required (VERSION 2.6)
project (AtomVM)

option(ADVANCED_TRACING "Enable advanced tracing" OFF)
if (ADVANCED_TRACING)
    add_definitions(-DENABLE_ADVANCED_TRACE)
endif()

add_subdirectory(libAtomVM)

if((${CMAKE_SYSTEM_NAME} STREQUAL "Darwin") OR
   (${CMAKE_SYSTEM_NAME} STREQUAL "Linux") OR
   (${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD"))
    add_subdirectory(platforms/generic_unix)
else()
    message(FATAL_ERROR "Unsupported platform: ${CMAKE_SYSTEM_NAME}")
endif()

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

if(CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pedantic -Wextra -ggdb")
endif()

include_directories(${CMAKE_CURRENT_BINARY_DIR} libAtomVM/)

add_executable(AtomVM main.c)
target_link_libraries(AtomVM libAtomVM libAtomVM${PLATFORM_LIB_SUFFIX})
set_property(TARGET AtomVM PROPERTY C_STANDARD 99)

if (CMAKE_BUILD_TYPE STREQUAL "Coverage")
    set_target_properties(AtomVM PROPERTIES COMPILE_FLAGS "-O0 -fprofile-arcs -ftest-coverage")
endif()
