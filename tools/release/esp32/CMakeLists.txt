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

cmake_minimum_required (VERSION 2.6)
project (ReleaseEsp32)

include(${CMAKE_SOURCE_DIR}/CMakeModules/GetVersion.cmake)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/mkimage.erl ${CMAKE_CURRENT_BINARY_DIR}/mkimage.erl @COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/mkimage.sh.in ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/mkimage.sh @ONLY)
file(COPY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/mkimage.sh
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)
