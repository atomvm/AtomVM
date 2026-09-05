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

# Usage:
#   cmake -DAVM_IN=<file> -DAVM_OUT=<file.c> -P embed_avm.cmake

if(NOT AVM_OUT)
    message(FATAL_ERROR "AVM_OUT is required")
endif()

if(AVM_IN AND EXISTS "${AVM_IN}")
    file(READ "${AVM_IN}" hex HEX)
    string(LENGTH "${hex}" hex_len)
else()
    set(hex "")
    set(hex_len 0)
endif()

set(body "")
if(hex_len GREATER 0)
    string(REGEX REPLACE "([0-9a-fA-F][0-9a-fA-F])" "0x\\1," hex "${hex}")
    set(body "${hex}")
    set(size_expr "sizeof(embedded_avm)")
else()
    # Empty arrays are invalid in C; keep a dummy byte and advertise size 0.
    set(body "0")
    set(size_expr "0")
endif()

file(WRITE "${AVM_OUT}"
"#include <stddef.h>
#include <stdint.h>

const uint8_t embedded_avm[] = {${body}};
const size_t embedded_avm_size = ${size_expr};
")
