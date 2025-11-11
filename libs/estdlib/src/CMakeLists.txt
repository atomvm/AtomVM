#
# This file is part of AtomVM.
#
# Copyright 2018-2021 Fred Dushin <fred@dushin.net>
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

project(estdlib)

include(BuildErlang)

set(ERLANG_MODULES
    application
    base64
    binary
    calendar
    code
    code_server
    crypto
    dist_util
    erl_epmd
    erpc
    erts_debug
    ets
    file
    gen
    gen_event
    gen_server
    gen_statem
    gen_udp
    gen_udp_inet
    gen_udp_socket
    gen_tcp
    gen_tcp_inet
    gen_tcp_socket
    supervisor
    kernel
    net_kernel
    net_kernel_sup
    inet
    init
    io_lib
    io
    lists
    maps
    math
    net
    proc_lib
    sys
    file
    logger
    logger_std_h
    proplists
    queue
    sets
    socket
    socket_dist
    socket_dist_controller
    ssl
    string
    timer
    unicode
    erlang
)

pack_precompiled_archive(estdlib ${ERLANG_MODULES})

include(../../../version.cmake)

set(ESTDLIB_VERSION ${ATOMVM_BASE_VERSION})

install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/beams/
    DESTINATION lib/atomvm/lib/estdlib-${ESTDLIB_VERSION}/ebin
    FILES_MATCHING PATTERN "*.beam"
)

install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/estdlib.avm
    DESTINATION lib/atomvm/lib/estdlib-${ESTDLIB_VERSION}/ebin/
)

install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/
    DESTINATION lib/atomvm/lib/estdlib-${ESTDLIB_VERSION}/src
    FILES_MATCHING PATTERN "*.erl"
)
