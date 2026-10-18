# Copyright 2026 Peter M. <petermm@gmail.com>
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

if(NOT RTEMS_OBJCOPY)
    message(FATAL_ERROR "ESP32 image generation requires the RTEMS objcopy tool")
endif()

get_filename_component(_rtems_image_name "${PROJECT_EXECUTABLE}" NAME_WE)
set(_rtems_image_bin "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${_rtems_image_name}.bin")

add_custom_command(
    OUTPUT ${_rtems_image_bin}
    COMMAND ${RTEMS_OBJCOPY} -O binary
        $<TARGET_FILE:${PROJECT_EXECUTABLE}> ${_rtems_image_bin}
    DEPENDS $<TARGET_FILE:${PROJECT_EXECUTABLE}>
    COMMENT "Generating ESP32 direct-boot binary ${_rtems_image_bin}"
    VERBATIM
)
add_custom_target(${PROJECT_NAME}-bin ALL DEPENDS ${_rtems_image_bin})
message(STATUS "ESP32 direct-boot image: ${_rtems_image_bin}")

find_program(ESPTOOL NAMES esptool.py esptool)
if(ESPTOOL)
    set(_esp_port "$ENV{ESPPORT}")
    if(NOT _esp_port)
        set(_esp_port "/dev/ttyACM0")
    endif()
    add_custom_target(flash
        COMMAND ${ESPTOOL} --port ${_esp_port} --baud 921600 write_flash 0x0 ${_rtems_image_bin}
        DEPENDS ${_rtems_image_bin}
        COMMENT "Flashing ${_rtems_image_bin} to ESP32 via ${ESPTOOL} on ${_esp_port}"
        USES_TERMINAL
    )
endif()

find_program(ESP_EMU NAMES esp-emu)
if(ESP_EMU)
    add_custom_target(simulate
        COMMAND ${ESP_EMU} --chip ${RTEMS_BOARD} --firmware ${_rtems_image_bin}
        DEPENDS ${_rtems_image_bin}
        COMMENT "Running ${RTEMS_BOARD} firmware simulation with esp-emu"
        USES_TERMINAL
    )
endif()
