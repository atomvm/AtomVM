# Copyright 2026 Peter M. <petermm@gmail.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

# Standalone: cmake -S src/platforms/rtems/tests/npl -B /tmp/npl-tests
cmake_minimum_required(VERSION 3.13)
project(RtemsNplTests C)
enable_testing()
add_executable(test_npl test_npl.c)
target_include_directories(test_npl PRIVATE mock ../../bt/include)
target_compile_features(test_npl PRIVATE c_std_11)
target_compile_options(test_npl PRIVATE -Wall -Wextra -Werror -Wno-unused-parameter)
option(NPL_SANITIZERS "Enable address and undefined behavior sanitizers" OFF)
if(NPL_SANITIZERS)
    target_compile_options(test_npl PRIVATE -fsanitize=address,undefined -fno-omit-frame-pointer)
    target_link_options(test_npl PRIVATE -fsanitize=address,undefined)
endif()
add_test(NAME npl COMMAND test_npl)
