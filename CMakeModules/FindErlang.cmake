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

cmake_minimum_required(VERSION 3.13)

find_program(ERLC_EXECUTABLE erlc)
find_program(ERL_EXECUTABLE erl)

if (ERLC_EXECUTABLE AND ERL_EXECUTABLE)
    execute_process(COMMAND erl -eval "io:put_chars(erlang:system_info(otp_release))." -s init stop -noshell OUTPUT_VARIABLE Erlang_VERSION)
endif()

include(FindPackageHandleStandardArgs)

if (CMAKE_VERSION VERSION_LESS 3.19)
    # Handle version range ourselves
    if (Erlang_FIND_VERSION AND ERL_EXECUTABLE)
        string(REPLACE "..." ";" Erlang_FIND_VERSION_LIST ${Erlang_FIND_VERSION})
        list(LENGTH Erlang_FIND_VERSION_LIST Erlang_FIND_VERSION_LIST_LEN)
        if (Erlang_FIND_VERSION_LIST_LEN EQUAL 1)
            find_package_handle_standard_args(Erlang
                FOUND_VAR Erlang_FOUND
                REQUIRED_VARS ERLC_EXECUTABLE ERL_EXECUTABLE
                VERSION_VAR Erlang_VERSION
            )
        elseif(${Erlang_FIND_VERSION_LIST_LEN} EQUAL 2)
            list(GET Erlang_FIND_VERSION_LIST 0 Erlang_FIND_VERSION_MIN)
            list(GET Erlang_FIND_VERSION_LIST 1 Erlang_FIND_VERSION_MAX)
            if (${Erlang_VERSION} LESS Erlang_FIND_VERSION_MIN)
                message(FATAL_ERROR "-- Found Erlang: ${ERL_EXECUTABLE} but OTP Release ${Erlang_VERSION} is less than required ${Erlang_FIND_VERSION_MIN}")
            endif()
            if (${Erlang_VERSION} GREATER Erlang_FIND_VERSION_MAX)
                message(FATAL_ERROR "-- Found Erlang: ${ERL_EXECUTABLE} but OTP Release ${Erlang_VERSION} is greater than supported ${Erlang_FIND_VERSION_MAX}")
            endif()
            find_package_handle_standard_args(Erlang
              FOUND_VAR Erlang_FOUND
              REQUIRED_VARS ERLC_EXECUTABLE ERL_EXECUTABLE
            )
        else()
            message(FATAL_ERROR "-- Found Erlang: ${ERL_EXECUTABLE} but version range syntax is incorrect ${Erlang_FIND_VERSION}")
        endif()
    else()
        find_package_handle_standard_args(Erlang
          FOUND_VAR Erlang_FOUND
          REQUIRED_VARS ERLC_EXECUTABLE ERL_EXECUTABLE
        )
    endif()
else()
    find_package_handle_standard_args(Erlang
        FOUND_VAR Erlang_FOUND
        HANDLE_VERSION_RANGE
        REQUIRED_VARS ERLC_EXECUTABLE ERL_EXECUTABLE
        VERSION_VAR Erlang_VERSION
   )
endif()
