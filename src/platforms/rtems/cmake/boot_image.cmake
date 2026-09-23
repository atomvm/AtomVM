# Copyright 2026 Peter M. <petermm@gmail.com>
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

if(NOT RTEMS_ARCH STREQUAL "arm")
    message(FATAL_ERROR "RTEMS_CREATE_ZIMAGE currently requires RTEMS_ARCH=arm")
endif()
if(NOT RTEMS_OBJCOPY)
    message(FATAL_ERROR "RTEMS_CREATE_ZIMAGE requires the RTEMS objcopy tool")
endif()

find_program(RTEMS_MKIMAGE NAMES mkimage mkimage.py)
if(NOT RTEMS_MKIMAGE)
    message(FATAL_ERROR
        "RTEMS_CREATE_ZIMAGE requires mkimage (install u-boot-tools)")
endif()
find_program(RTEMS_GZIP NAMES gzip)
if(NOT RTEMS_GZIP)
    message(FATAL_ERROR "RTEMS_CREATE_ZIMAGE requires gzip")
endif()

if(NOT RTEMS_IMAGE_LOAD_ADDRESS)
    message(FATAL_ERROR "RTEMS_CREATE_ZIMAGE requires RTEMS_IMAGE_LOAD_ADDRESS")
endif()
include(${CMAKE_CURRENT_LIST_DIR}/validate_dtb.cmake)
set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS "${RTEMS_DTB}")

get_filename_component(_rtems_image_name "${PROJECT_EXECUTABLE}" NAME_WE)
set(_rtems_image_stem "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${_rtems_image_name}")
set(_rtems_image_bin "${_rtems_image_stem}.bin")
set(_rtems_image_gz "${_rtems_image_bin}.gz")
set(_rtems_image_z "${_rtems_image_stem}.zImage")
set(_rtems_oftree "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/oftree")

add_custom_command(
    OUTPUT ${_rtems_oftree}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${RTEMS_DTB} ${_rtems_oftree}
    DEPENDS ${RTEMS_DTB}
    COMMENT "Copying the board device tree to oftree"
    VERBATIM
)

add_custom_command(
    OUTPUT ${_rtems_image_bin}
    COMMAND ${RTEMS_OBJCOPY} -O binary
        $<TARGET_FILE:${PROJECT_EXECUTABLE}> ${_rtems_image_bin}
    # CMake 3.28 can treat a bare .exe target name as a relative file path.
    DEPENDS $<TARGET_FILE:${PROJECT_EXECUTABLE}>
    COMMENT "Converting ${PROJECT_EXECUTABLE} to a flat binary"
    VERBATIM
)
add_custom_command(
    OUTPUT ${_rtems_image_gz}
    COMMAND ${RTEMS_GZIP} -n -9 -f -k ${_rtems_image_bin}
    DEPENDS ${_rtems_image_bin}
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
    COMMENT "Compressing ${_rtems_image_bin}"
    VERBATIM
)
add_custom_command(
    OUTPUT ${_rtems_image_z}
    COMMAND ${RTEMS_MKIMAGE}
        -A arm -O linux -T kernel -C gzip
        -a ${RTEMS_IMAGE_LOAD_ADDRESS}
        -e ${RTEMS_IMAGE_LOAD_ADDRESS}
        -n RTEMS -d ${_rtems_image_gz} ${_rtems_image_z}
    DEPENDS ${_rtems_image_gz}
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
    COMMENT "Creating ${_rtems_image_z}"
    VERBATIM
)
add_custom_target(${PROJECT_NAME}-zimage ALL DEPENDS ${_rtems_image_z} ${_rtems_oftree})
message(STATUS "Boot image: ${_rtems_image_z}")
message(STATUS "Board device tree: ${_rtems_oftree}")
