# Copyright 2026 Peter M. <petermm@gmail.com>
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

# Standalone host tests, without an RTEMS toolchain.
cmake_minimum_required(VERSION 3.13)
project(RtemsAppLoaderTests C)
enable_testing()
set(RTEMS_TEST_PACK "" CACHE FILEPATH "Runnable AVM pack with the startup module first")
if(NOT EXISTS "${RTEMS_TEST_PACK}")
    message(FATAL_ERROR "Set RTEMS_TEST_PACK to a runnable AVM pack with the startup module first")
endif()
add_executable(test_app_loader test_app_loader.c ../src/app_loader.c)
target_include_directories(test_app_loader PRIVATE ../src)
target_compile_features(test_app_loader PRIVATE c_std_11)
target_compile_options(test_app_loader PRIVATE -Wall -Wextra -Werror)
add_test(NAME app_loader COMMAND test_app_loader "${RTEMS_TEST_PACK}")
