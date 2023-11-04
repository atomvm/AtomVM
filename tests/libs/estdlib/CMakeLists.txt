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

project(test_estdlib)

include(BuildErlang)

set(ERLANG_MODULES
    test_calendar
    test_gen_event
    test_gen_server
    test_gen_statem
    test_gen_udp
    test_gen_tcp
    test_io_lib
    test_lists
    test_logger
    test_maps
    test_net
    test_spawn
    test_ssl
    test_string
    test_proplists
    test_timer
    test_supervisor
    test_tcp_socket
    test_udp_socket
    notify_init_server
    ping_pong_server
)

pack_archive(test_estdlib_lib ${ERLANG_MODULES})
pack_test(test_estdlib estdlib eavmlib etest)
