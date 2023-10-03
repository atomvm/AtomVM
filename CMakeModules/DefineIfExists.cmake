#
# This file is part of AtomVM.
#
# Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

include(CheckSymbolExists)
include(CheckCSourceCompiles)

function(define_if_symbol_exists target symbol header scope macro)
    if (NOT DEFINED ${macro})
        check_symbol_exists(${symbol} ${header} ${macro})
        if (${macro})
            target_compile_definitions(${target} ${scope} ${macro})
        endif(${macro})
    endif()
endfunction()
function(define_if_function_exists target symbol header scope macro)
    if (NOT DEFINED ${macro})
        check_c_source_compiles("
        #include <${header}>
        int main(int argc)
        {
              return ((int*)(&${symbol}))[argc];
        }" ${macro})
        if (${macro})
            target_compile_definitions(${target} ${scope} ${macro})
        endif(${macro})
    endif()
endfunction()
