cmake_minimum_required(VERSION 3.9)
project(atom-vm)

# Include an error in case the user forgets to specify ARM as a toolchain
if (NOT CMAKE_TOOLCHAIN_FILE)
    message(FATAL_ERROR "Cross compiling only. Please use -DCMAKE_TOOLCHAIN_FILE=cmake/arm-toolchain.cmake or use\
    your own toolchain file")
endif ()

if ((NOT ${CMAKE_C_COMPILER_ID} STREQUAL "GNU") OR
    (NOT ${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU") OR
    (${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS 7.2.1))
    message(WARNING "This project is targeted for ARM GCC versions 7.2.1 or above. Some features may not be supported\
     by your compiler. You are using:
     ${CMAKE_CXX_COMPILER_ID} C, version ${CMAKE_C_COMPILER_VERSION}
     ${CMAKE_CXX_COMPILER_ID} CXX, version ${CMAKE_CXX_COMPILER_VERSION}")
endif()

# Set the C/C++ standards
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/build)

# Verbose logging
set(LOG_VERBOSE false)

if (NOT DEVICE)
    set(DEVICE stm32f407vgt6)
endif ()

# Include libopencm3
include(cmake/libopencm3.cmake)

# Include additional compilation flags
include(cmake/compile-flags.cmake)

# Collect sources into SOURCE_FILES variable and include your headers directory
file(GLOB_RECURSE SOURCE_FILES "src/*.c" "src/*.cpp")
include_directories(src)

# Specify output executable
add_executable(${PROJECT_NAME}.elf ${SOURCE_FILES})

add_subdirectory(../../libAtomVM ./libAtomVM)
target_include_directories(${PROJECT_NAME}.elf PRIVATE ../../libAtomVM)
target_link_libraries(${PROJECT_NAME}.elf "libAtomVM")

# Output elf file size
add_custom_command(
    TARGET ${PROJECT_NAME}.elf
    POST_BUILD
    COMMAND ${ARM_SIZE} ${PROJECT_NAME}.elf
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
)
