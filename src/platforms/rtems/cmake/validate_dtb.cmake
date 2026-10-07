# Copyright 2026 Peter M. <petermm@gmail.com>
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

# Also usable without a cross compiler:
# cmake -DRTEMS_DTB=/path/to/board.dtb -DRTEMS_BOARD_FDT_COMPATIBLE=vendor,board \
#     -P validate_dtb.cmake
if(NOT RTEMS_BOARD_FDT_COMPATIBLE)
    message(FATAL_ERROR "DTB validation requires RTEMS_BOARD_FDT_COMPATIBLE")
endif()
if(NOT EXISTS "${RTEMS_DTB}")
    message(FATAL_ERROR "DTB not found: ${RTEMS_DTB}")
endif()

file(READ "${RTEMS_DTB}" _rtems_dtb_magic LIMIT 4 HEX)
if(NOT _rtems_dtb_magic STREQUAL "d00dfeed")
    message(FATAL_ERROR "Invalid DTB header: ${RTEMS_DTB}")
endif()

find_program(RTEMS_FDTGET NAMES fdtget)
if(NOT RTEMS_FDTGET)
    message(FATAL_ERROR "DTB validation requires fdtget (install device-tree-compiler or dtc)")
endif()
execute_process(
    COMMAND ${RTEMS_FDTGET} -t s "${RTEMS_DTB}" / compatible
    RESULT_VARIABLE _rtems_dtb_result
    OUTPUT_VARIABLE _rtems_dtb_compatible
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_VARIABLE _rtems_dtb_error
)
string(REPLACE " " ";" _rtems_dtb_compatible "${_rtems_dtb_compatible}")
list(FIND _rtems_dtb_compatible "${RTEMS_BOARD_FDT_COMPATIBLE}" _rtems_dtb_match)
if(NOT _rtems_dtb_result EQUAL 0 OR _rtems_dtb_match EQUAL -1)
    message(FATAL_ERROR
        "DTB must have root compatible '${RTEMS_BOARD_FDT_COMPATIBLE}': ${RTEMS_DTB}\n${_rtems_dtb_error}")
endif()
