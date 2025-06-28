#
# This file is part of AtomVM.
#
# Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

include(FetchContent)

FetchContent_Declare(
  mbedtls
  GIT_REPOSITORY http://github.com/mbed-TLS/mbedtls.git
  GIT_TAG        v3.6.3.1
  GIT_SHALLOW    1
)

include(CheckCompilerFlag)
check_compiler_flag(C -Wno-unterminated-string-initialization compiler_supports_unterminated_string_initialization)

if (${compiler_supports_unterminated_string_initialization})
    get_property(
        compile_options
        DIRECTORY
        PROPERTY COMPILE_OPTIONS
    )

    set_property(
        DIRECTORY
        APPEND
        PROPERTY COMPILE_OPTIONS -Wno-unterminated-string-initialization
    )

    FetchContent_MakeAvailable(mbedtls)

    set_property(
        DIRECTORY
        PROPERTY COMPILE_OPTIONS ${compile_options}
    )

    unset(compile_options)
else()
    FetchContent_MakeAvailable(mbedtls)
endif ()

unset(compiler_supports_unterminated_string_initialization)
