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
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

function(avm_get_system_architecture_string out_var)
    set(options)
    set(one_value_args PLATFORM_VENDOR PLATFORM_OS)
    cmake_parse_arguments(PARSE_ARGV 1 AVM "${options}" "${one_value_args}" "")

    execute_process(
        COMMAND ${CMAKE_C_COMPILER} -dumpmachine
        OUTPUT_VARIABLE avm_raw_system_architecture
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )

    if (avm_raw_system_architecture STREQUAL "")
        unset(${out_var} PARENT_SCOPE)
        return()
    endif()

    string(REPLACE "-" ";" avm_system_architecture_parts "${avm_raw_system_architecture}")
    list(LENGTH avm_system_architecture_parts avm_system_architecture_length)

    if (avm_system_architecture_length EQUAL 1)
        list(GET avm_system_architecture_parts 0 avm_architecture)
        if (DEFINED AVM_PLATFORM_VENDOR)
            set(avm_vendor "${AVM_PLATFORM_VENDOR}")
        else()
            set(avm_vendor "unknown")
        endif()
        set(avm_os "unknown")
    elseif (avm_system_architecture_length EQUAL 2)
        list(GET avm_system_architecture_parts 0 avm_architecture)
        if (DEFINED AVM_PLATFORM_VENDOR)
            set(avm_vendor "${AVM_PLATFORM_VENDOR}")
        else()
            set(avm_vendor "unknown")
        endif()
        list(GET avm_system_architecture_parts 1 avm_os)
    else()
        list(GET avm_system_architecture_parts 0 avm_architecture)
        list(GET avm_system_architecture_parts 1 avm_vendor)
        if (DEFINED AVM_PLATFORM_VENDOR AND (avm_vendor STREQUAL "none" OR avm_vendor STREQUAL "unknown"))
            set(avm_vendor "${AVM_PLATFORM_VENDOR}")
        endif()

        list(REMOVE_AT avm_system_architecture_parts 0 1)
        string(REPLACE ";" "_" avm_os "${avm_system_architecture_parts}")
    endif()

    if (DEFINED AVM_PLATFORM_OS)
        set(avm_os "${AVM_PLATFORM_OS}")
    endif()

    string(REPLACE "-" "_" avm_architecture "${avm_architecture}")
    string(REPLACE "-" "_" avm_vendor "${avm_vendor}")
    string(REPLACE "-" "_" avm_os "${avm_os}")

    set(${out_var} "${avm_architecture}-${avm_vendor}-${avm_os}" PARENT_SCOPE)
endfunction()
