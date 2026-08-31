#
# This file is part of AtomVM.
#
# Copyright 2026 Peter M. <petermm@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

# Cross-compiling toolchain for RTEMS 6.x applications.
#
# Required:
#   RTEMS_PREFIX  Installation prefix (or env RTEMS_PREFIX)
#
# Optional:
#   RTEMS_BSP     Architecture/BSP, default sparc/erc32
#   RTEMS_VERSION Major tools version, default 6

set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)
# Nested CMake try_compile (compiler ABI / feature tests) does not inherit
# cache variables unless they are listed here. Without this, RTEMS_PREFIX is
# empty in the test project and configuration fails.
set(CMAKE_TRY_COMPILE_PLATFORM_VARIABLES RTEMS_PREFIX RTEMS_BSP RTEMS_VERSION)

if(NOT RTEMS_PREFIX)
    if(DEFINED ENV{RTEMS_PREFIX})
        set(RTEMS_PREFIX "$ENV{RTEMS_PREFIX}")
    endif()
endif()
if(NOT RTEMS_PREFIX)
    message(FATAL_ERROR "RTEMS_PREFIX is not set. Pass -DRTEMS_PREFIX=/path/to/rtems/6.2")
endif()

if(NOT RTEMS_BSP)
    if(DEFINED ENV{RTEMS_BSP})
        set(RTEMS_BSP "$ENV{RTEMS_BSP}")
    else()
        set(RTEMS_BSP "sparc/erc32")
    endif()
endif()

if(NOT RTEMS_VERSION)
    if(DEFINED ENV{RTEMS_VERSION})
        set(RTEMS_VERSION "$ENV{RTEMS_VERSION}")
    else()
        set(RTEMS_VERSION "6")
    endif()
endif()

string(REPLACE "/" ";" _rtems_bsp_parts "${RTEMS_BSP}")
list(LENGTH _rtems_bsp_parts _rtems_bsp_parts_len)
if(NOT _rtems_bsp_parts_len EQUAL 2)
    message(FATAL_ERROR "RTEMS_BSP must be of the form <arch>/<bsp> (got '${RTEMS_BSP}')")
endif()
list(GET _rtems_bsp_parts 0 RTEMS_ARCH)
list(GET _rtems_bsp_parts 1 RTEMS_BSP_NAME)

set(RTEMS_TARGET "${RTEMS_ARCH}-rtems${RTEMS_VERSION}")
set(CMAKE_SYSTEM_PROCESSOR "${RTEMS_ARCH}")

set(RTEMS_BIN_PATH "${RTEMS_PREFIX}/bin")
if(NOT IS_DIRECTORY "${RTEMS_BIN_PATH}")
    message(FATAL_ERROR "RTEMS tools not found at ${RTEMS_BIN_PATH}")
endif()

find_program(RTEMS_CC ${RTEMS_TARGET}-gcc HINTS "${RTEMS_BIN_PATH}" REQUIRED)
find_program(RTEMS_CXX ${RTEMS_TARGET}-g++ HINTS "${RTEMS_BIN_PATH}")
find_program(RTEMS_AR ${RTEMS_TARGET}-ar HINTS "${RTEMS_BIN_PATH}" REQUIRED)
find_program(RTEMS_AS ${RTEMS_TARGET}-as HINTS "${RTEMS_BIN_PATH}")
find_program(RTEMS_NM ${RTEMS_TARGET}-nm HINTS "${RTEMS_BIN_PATH}")
find_program(RTEMS_OBJCOPY ${RTEMS_TARGET}-objcopy HINTS "${RTEMS_BIN_PATH}")
find_program(RTEMS_SIZE ${RTEMS_TARGET}-size HINTS "${RTEMS_BIN_PATH}")
find_program(RTEMS_RANLIB ${RTEMS_TARGET}-ranlib HINTS "${RTEMS_BIN_PATH}")
find_program(RTEMS_STRIP ${RTEMS_TARGET}-strip HINTS "${RTEMS_BIN_PATH}")

set(CMAKE_C_COMPILER "${RTEMS_CC}")
if(RTEMS_CXX)
    set(CMAKE_CXX_COMPILER "${RTEMS_CXX}")
endif()
set(CMAKE_ASM_COMPILER "${RTEMS_CC}")
set(CMAKE_AR "${RTEMS_AR}")
if(RTEMS_RANLIB)
    set(CMAKE_RANLIB "${RTEMS_RANLIB}")
endif()

set(CMAKE_FIND_ROOT_PATH "${RTEMS_PREFIX}")
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)

set(_rtems_pkg_name "${RTEMS_TARGET}-${RTEMS_BSP_NAME}")
set(_rtems_pkg_paths
    "${RTEMS_PREFIX}/lib/pkgconfig"
    "${RTEMS_PREFIX}/${RTEMS_TARGET}/${RTEMS_BSP_NAME}/lib/pkgconfig"
)

set(_saved_pkg_config_path "$ENV{PKG_CONFIG_PATH}")
set(_pkg_config_path "${_saved_pkg_config_path}")
foreach(_pkg_path ${_rtems_pkg_paths})
    if(IS_DIRECTORY "${_pkg_path}")
        if(_pkg_config_path)
            set(_pkg_config_path "${_pkg_path}:${_pkg_config_path}")
        else()
            set(_pkg_config_path "${_pkg_path}")
        endif()
    endif()
endforeach()
set(ENV{PKG_CONFIG_PATH} "${_pkg_config_path}")

find_program(PKG_CONFIG_EXECUTABLE pkg-config)
set(RTEMS_BSP_CFLAGS "")
set(RTEMS_BSP_LDFLAGS "")
if(PKG_CONFIG_EXECUTABLE)
    execute_process(
        COMMAND ${PKG_CONFIG_EXECUTABLE} --cflags ${_rtems_pkg_name}
        OUTPUT_VARIABLE RTEMS_BSP_CFLAGS
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE _rtems_pkg_cflags_result
    )
    execute_process(
        COMMAND ${PKG_CONFIG_EXECUTABLE} --libs ${_rtems_pkg_name}
        OUTPUT_VARIABLE RTEMS_BSP_LDFLAGS
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE _rtems_pkg_libs_result
    )
    if(NOT _rtems_pkg_cflags_result EQUAL 0)
        set(RTEMS_BSP_CFLAGS "")
    endif()
    if(NOT _rtems_pkg_libs_result EQUAL 0)
        set(RTEMS_BSP_LDFLAGS "")
    endif()
endif()
set(ENV{PKG_CONFIG_PATH} "${_saved_pkg_config_path}")

if(NOT RTEMS_BSP_CFLAGS OR NOT RTEMS_BSP_LDFLAGS)
    set(_rtems_bsp_libdir "${RTEMS_PREFIX}/${RTEMS_TARGET}/${RTEMS_BSP_NAME}/lib")
    if(NOT IS_DIRECTORY "${_rtems_bsp_libdir}")
        message(FATAL_ERROR
            "Could not find pkg-config flags for ${_rtems_pkg_name} and BSP libdir ${_rtems_bsp_libdir} is missing. "
            "Is RTEMS ${RTEMS_VERSION} installed at ${RTEMS_PREFIX} with BSP ${RTEMS_BSP}?")
    endif()
    # RTEMS 6 no longer installs bsp_specs; -B plus -qrtems is enough.
    set(RTEMS_BSP_CFLAGS "-B${_rtems_bsp_libdir}/ -qrtems")
    set(RTEMS_BSP_LDFLAGS "-B${_rtems_bsp_libdir}/ -qrtems")
    message(STATUS "Using RTEMS BSP fallback flags from ${_rtems_bsp_libdir}")
else()
    message(STATUS "Using RTEMS pkg-config module ${_rtems_pkg_name}")
endif()

string(STRIP "${RTEMS_BSP_CFLAGS}" RTEMS_BSP_CFLAGS)
string(STRIP "${RTEMS_BSP_LDFLAGS}" RTEMS_BSP_LDFLAGS)

set(CMAKE_C_FLAGS_INIT "${RTEMS_BSP_CFLAGS}")
set(CMAKE_CXX_FLAGS_INIT "${RTEMS_BSP_CFLAGS}")
set(CMAKE_EXE_LINKER_FLAGS_INIT "${RTEMS_BSP_LDFLAGS}")

set(RTEMS_PREFIX "${RTEMS_PREFIX}" CACHE PATH "RTEMS installation prefix" FORCE)
set(RTEMS_BSP "${RTEMS_BSP}" CACHE STRING "RTEMS architecture/BSP (arch/bsp)" FORCE)
set(RTEMS_VERSION "${RTEMS_VERSION}" CACHE STRING "RTEMS major version" FORCE)
set(RTEMS_ARCH "${RTEMS_ARCH}" CACHE STRING "RTEMS architecture" FORCE)
set(RTEMS_BSP_NAME "${RTEMS_BSP_NAME}" CACHE STRING "RTEMS BSP name" FORCE)
set(RTEMS_TARGET "${RTEMS_TARGET}" CACHE STRING "RTEMS GNU target triple" FORCE)
