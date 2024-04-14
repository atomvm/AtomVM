#
# This file is part of AtomVM.
#
# Copyright 2024 Winford (Uncle  Grumpy) <winford@object.stream>
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

function( version_from_git )

  # Find Git
  find_package(Git)
  if(!Git_FOUND)
    message( FATAL_ERROR "Git not found" )
  endif(!Git_FOUND)

  # Git tag
  execute_process(
    COMMAND           "${GIT_EXECUTABLE}" describe --exact-match
    WORKING_DIRECTORY "${LIBOPENCM3_DIR}"
    RESULT_VARIABLE   git_result
    OUTPUT_VARIABLE   git_tag
    ERROR_VARIABLE    git_error
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_STRIP_TRAILING_WHITESPACE
  )
  if( NOT git_result EQUAL 0 )
    message( FATAL_ERROR "Failed to execute Git: ${git_error}" )
  endif()

  if( git_tag MATCHES "^v([0-9]+)[.]([0-9]+)[.]([0-9]+).*$" )
    set( ide_version_major "${CMAKE_MATCH_1}" )
    set( ide_version_minor "${CMAKE_MATCH_2}" )
    set( ide_version_patch "${CMAKE_MATCH_3}" )
  else()
    set( ide_version_major "undefined" )
    set( ide_version_minor "undefined" )
    set( ide_version_patch "undefined" )
  endif()

  # Set parent scope variables
  set( IDE_VERSION_MAJOR ${ide_version_major} PARENT_SCOPE)
  set( IDE_VERSION_MINOR ${ide_version_minor} PARENT_SCOPE)
  set( IDE_VERSION_PATCH ${ide_version_patch} PARENT_SCOPE)

endfunction( version_from_git )
