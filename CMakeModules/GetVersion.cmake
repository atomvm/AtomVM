#
# This file is part of AtomVM.
#
# Copyright 2022 Fred Dushin <fred@dushin.net>
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

include(${CMAKE_SOURCE_DIR}/version.cmake)

if (ATOMVM_DEV)
    set(ATOMVM_GIT_REVISION "<unknown>")
    execute_process(
        COMMAND git rev-parse --short HEAD
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        OUTPUT_VARIABLE ATOMVM_GIT_REVISION
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    if (NOT ATOMVM_GIT_REVISION STREQUAL "")
        set(ATOMVM_VERSION "${ATOMVM_BASE_VERSION}+git.${ATOMVM_GIT_REVISION}")
    else()
        set(ATOMVM_VERSION ${ATOMVM_BASE_VERSION})
    endif()
else()
    set(ATOMVM_VERSION ${ATOMVM_BASE_VERSION})
endif()
