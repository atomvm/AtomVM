#
# This file is part of AtomVM.
#
# Copyright 2026 Peter M <petermm@gmail.com>
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

cmake_minimum_required (VERSION 3.20)
set(CMAKE_C_STANDARD_REQUIRED YES)
set(CMAKE_CXX_STANDARD_REQUIRED YES)

# Enforce to disable any compiler-specific extensions
set(CMAKE_C_EXTENSIONS NO)
set(CMAKE_CXX_EXTENSIONS NO)
set(C_STANDARD_REQUIRED YES)
set_property(GLOBAL PROPERTY C_STANDARD_REQUIRED TRUE)
set_property(GLOBAL PROPERTY CSTD iso9899:2011)
set_property(GLOBAL PROPERTY C_STANDARD iso9899:2011)

get_filename_component(ATOMVM_ZEPHYR_ROOT "${CMAKE_CURRENT_LIST_DIR}/.." ABSOLUTE)

option(AVM_ZEPHYR_NATIVE_SIM_I2C_TEST "Run the native_sim Erlang I2C emulator test" OFF)

# Reuse the normal platform configuration, plus any matching board-specific
# config or overlay from the Zephyr port root. Zephyr normally discovers these
# relative to the application root, but this test app lives one directory down.
set(CONF_FILE "${ATOMVM_ZEPHYR_ROOT}/prj.conf")
set(ATOMVM_ZEPHYR_BOARD_CANDIDATES "${BOARD}")
set(ATOMVM_ZEPHYR_TEST_BOARD_CONFS)
if(BOARD MATCHES "/")
    string(REGEX REPLACE "/.*" "" ATOMVM_ZEPHYR_BOARD_BASE "${BOARD}")
    list(APPEND ATOMVM_ZEPHYR_BOARD_CANDIDATES "${ATOMVM_ZEPHYR_BOARD_BASE}")
endif()

foreach(ATOMVM_ZEPHYR_BOARD_CANDIDATE ${ATOMVM_ZEPHYR_BOARD_CANDIDATES})
    string(REPLACE "/" "_" ATOMVM_ZEPHYR_BOARD_FILE "${ATOMVM_ZEPHYR_BOARD_CANDIDATE}")
    set(ATOMVM_ZEPHYR_BOARD_CONF "${ATOMVM_ZEPHYR_ROOT}/boards/${ATOMVM_ZEPHYR_BOARD_FILE}.conf")
    set(ATOMVM_ZEPHYR_BOARD_OVERLAY "${ATOMVM_ZEPHYR_ROOT}/boards/${ATOMVM_ZEPHYR_BOARD_FILE}.overlay")

    if(NOT DEFINED EXTRA_CONF_FILE AND EXISTS "${ATOMVM_ZEPHYR_BOARD_CONF}")
        set(EXTRA_CONF_FILE "${ATOMVM_ZEPHYR_BOARD_CONF}")
    endif()
    if(NOT DEFINED DTC_OVERLAY_FILE AND EXISTS "${ATOMVM_ZEPHYR_BOARD_OVERLAY}")
        set(DTC_OVERLAY_FILE "${ATOMVM_ZEPHYR_BOARD_OVERLAY}")
    endif()

    set(ATOMVM_ZEPHYR_TEST_BOARD_CONF "${CMAKE_CURRENT_LIST_DIR}/boards/${ATOMVM_ZEPHYR_BOARD_FILE}.conf")
    if(EXISTS "${ATOMVM_ZEPHYR_TEST_BOARD_CONF}")
        list(APPEND ATOMVM_ZEPHYR_TEST_BOARD_CONFS "${ATOMVM_ZEPHYR_TEST_BOARD_CONF}")
    endif()
    set(ATOMVM_ZEPHYR_TEST_BOARD_OVERLAY "${CMAKE_CURRENT_LIST_DIR}/boards/${ATOMVM_ZEPHYR_BOARD_FILE}.overlay")
    if(EXISTS "${ATOMVM_ZEPHYR_TEST_BOARD_OVERLAY}")
        list(APPEND DTC_OVERLAY_FILE "${ATOMVM_ZEPHYR_TEST_BOARD_OVERLAY}")
    endif()
endforeach()

list(APPEND EXTRA_CONF_FILE "${CMAKE_CURRENT_LIST_DIR}/prj.conf")
list(APPEND EXTRA_CONF_FILE ${ATOMVM_ZEPHYR_TEST_BOARD_CONFS})
list(APPEND DTC_OVERLAY_FILE "${CMAKE_CURRENT_LIST_DIR}/app.overlay")

if(AVM_ZEPHYR_NATIVE_SIM_I2C_TEST)
    list(APPEND EXTRA_CONF_FILE "${CMAKE_CURRENT_LIST_DIR}/native_sim_i2c.conf")
    list(APPEND DTC_OVERLAY_FILE "${CMAKE_CURRENT_LIST_DIR}/boards/native_sim_i2c.overlay")
endif()

if(BOARD MATCHES "^qemu_x86_64")
    set(AVM_ZEPHYR_EXPERIMENTAL_SMP ON CACHE BOOL "Enable experimental AtomVM SMP support on Zephyr" FORCE)
else()
    set(AVM_ZEPHYR_EXPERIMENTAL_SMP OFF CACHE BOOL "Enable experimental AtomVM SMP support on Zephyr" FORCE)
endif()

set(AVM_ZEPHYR_BOOT_TEST OFF CACHE BOOL "Boot AtomVM far enough to exercise Zephyr platform initialization" FORCE)
set(AVM_ZEPHYR_TEST_AVM ON CACHE BOOL "Use an embedded test AVM pack instead of a flashed application" FORCE)
set(AVM_ZEPHYR_TEST_EXIT ON CACHE BOOL "Exit the Zephyr application when the AtomVM application returns" FORCE)


set(MBEDTLS_FATAL_WARNINGS OFF CACHE BOOL "" FORCE)
find_package(Zephyr REQUIRED HINTS $ENV{ZEPHYR_BASE})
project(AtomVMZephyrTests LANGUAGES C)

include(${ATOMVM_ZEPHYR_ROOT}/cmake/AtomVMZephyrApp.cmake)

if(AVM_ZEPHYR_NATIVE_SIM_I2C_TEST)
    target_sources(app PRIVATE ${CMAKE_CURRENT_LIST_DIR}/src/i2c_native_sim_emul.c)
endif()

# Zephyr's fake PWM driver (used on native_sim) needs FFF globals linked in.
if(CONFIG_PWM_FAKE)
    target_sources(app PRIVATE ${CMAKE_CURRENT_LIST_DIR}/src/pwm_fake_fff.c)
endif()

add_subdirectory(test_erl_sources)
