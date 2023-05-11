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

project(examples_erlang)

include(BuildErlang)

add_subdirectory(esp32)
add_subdirectory(rp2040)

pack_runnable(hello_world hello_world)
pack_runnable(udp_server udp_server estdlib eavmlib)
pack_runnable(udp_client udp_client estdlib eavmlib)
pack_runnable(tcp_client tcp_client estdlib eavmlib)
pack_runnable(tcp_server tcp_server estdlib eavmlib)
pack_runnable(tcp_socket_client tcp_socket_client estdlib eavmlib)
pack_runnable(tcp_socket_server tcp_socket_server estdlib eavmlib)
pack_runnable(udp_socket_server udp_socket_server estdlib eavmlib)
pack_runnable(udp_socket_client udp_socket_client estdlib eavmlib)
pack_runnable(hello_world_server hello_world_server estdlib eavmlib)
pack_runnable(system_info_server system_info_server estdlib eavmlib)
pack_runnable(code_lock code_lock estdlib eavmlib)
pack_runnable(mqtt_client mqtt_client estdlib eavmlib)
pack_runnable(network_console network_console estdlib eavmlib alisp)
pack_runnable(logging_example logging_example estdlib eavmlib)
