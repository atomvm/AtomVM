cmake_minimum_required (VERSION 2.6)
project (libAtomVMPlatformGenericUnix)

include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_SOURCE_DIR}/src/libAtomVM)

if(${CMAKE_GENERATOR} STREQUAL "Xcode")
    set(HEADER_FILES
        generic_unix_sys.h
        mapped_file.h
        platform_defaultatoms.h
    )
endif()
set(SOURCE_FILES
    gpio_driver.c
    sys.c
    mapped_file.c
    network_driver.c
    platform_defaultatoms.c
    platform_nifs.c
    socket_driver.c
)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

if(CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pedantic -Wextra -ggdb")
endif()

add_library(libAtomVM${PLATFORM_LIB_SUFFIX} ${SOURCE_FILES} ${HEADER_FILES})
target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} libAtomVM)
set_property(TARGET libAtomVM${PLATFORM_LIB_SUFFIX} PROPERTY C_STANDARD 99)

if (CMAKE_BUILD_TYPE STREQUAL "Coverage")
    set_target_properties(libAtomVM${PLATFORM_LIB_SUFFIX} PROPERTIES COMPILE_FLAGS "-O0 -fprofile-arcs -ftest-coverage")
endif()
