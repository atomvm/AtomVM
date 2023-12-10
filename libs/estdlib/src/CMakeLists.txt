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
    base64
    binary
    calendar
    code
    crypto
    erts_debug
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
    inet
    io_lib
    io
    lists
    maps
    math
    net
    logger
    logger_std_h
    proplists
    socket
    ssl
    string
    timer
    unicode
    erlang
)

pack_archive(estdlib ${ERLANG_MODULES})
