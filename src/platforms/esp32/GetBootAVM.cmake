#
# This file is part of AtomVM.
#
# Copyright 2025 Winford (Uncle Grumpy) <winford@object.stream>
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

partition_table_get_partition_info(app_offset "--partition-name main.avm" "offset")
set(AVM_APP_OFFSET "${app_offset}")
partition_table_get_partition_info(lib_offset "--partition-name boot.avm" "offset")
set(AVM_LIB_OFFSET "${lib_offset}")
# partitions.csv and partitions-elixir.csv use 0x250000 for main.avm (non-JIT).
# partitions-jit.csv uses 0x300000 for main.avm (JIT: AOT boot.avm 1.375MB on 4MB flash).
# Use ATOMVM_ELIXIR_SUPPORT to select the elixir boot library flavor.
if ("${app_offset}" STREQUAL "0x250000")
    if (ATOMVM_ELIXIR_SUPPORT)
        set(BOOT_LIBS "elixir_esp32boot.avm")
        set(ATOMVM_FLAVOR "-elixir")
    else()
        set(BOOT_LIBS "esp32boot.avm")
        set(ATOMVM_FLAVOR "")
    endif()
elseif ("${app_offset}" STREQUAL "0x300000")
    # JIT partition layout: select arch-specific precompiled boot AVM.
    # AVM_JIT_TARGET_ARCH is set in components/libatomvm/CMakeLists.txt when
    # included from a component context; derive from IDF_TARGET otherwise.
    if (DEFINED AVM_JIT_TARGET_ARCH)
        set(_jit_arch "${AVM_JIT_TARGET_ARCH}")
    elseif (${IDF_TARGET} MATCHES "esp32c2|esp32c3|esp32c5|esp32c6|esp32c61|esp32h2|esp32p4")
        set(_jit_arch "riscv32")
    elseif (${IDF_TARGET} MATCHES "^esp32")
        set(_jit_arch "xtensa")
    else()
        set(_jit_arch "")
    endif()
    if (AVM_USE_32BIT_FLOAT AND _jit_arch STREQUAL "xtensa")
        set(_jit_variant "${_jit_arch}+float32")
    else()
        set(_jit_variant "${_jit_arch}")
    endif()
    if (_jit_variant)
        if (ATOMVM_ELIXIR_SUPPORT)
            set(BOOT_LIBS "elixir_esp32boot-${_jit_variant}.avm")
            set(ATOMVM_FLAVOR "-elixir-jit")
        else()
            set(BOOT_LIBS "esp32boot-${_jit_variant}.avm")
            set(ATOMVM_FLAVOR "-jit")
        endif()
    else()
        set(BOOT_LIBS "NONE")
        set(ATOMVM_FLAVOR "")
    endif()
else()
    set(BOOT_LIBS "NONE")
    set(ATOMVM_FLAVOR "")
endif()
