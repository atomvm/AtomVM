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
    test_apply
    test_binary
    test_calendar
    test_epmd
    test_gen_event
    test_gen_server
    test_gen_statem
    test_gen_udp
    test_gen_tcp
    test_inet
    test_io_lib
    test_json
    test_lists
    test_logger
    test_maps
    test_net
    test_net_kernel
    test_os
    test_serial_dist
    test_serial_dist_socat
    test_serial_dist_socat_peer
    test_serial_dist_beam_peer
    test_uart
    mock_uart_hal
    file_uart_hal
    test_proc_lib
    test_sets
    test_spawn
    test_ssl
    test_string
    test_sys
    test_proplists
    test_queue
    test_timer
    test_supervisor
    test_lists_subtraction
    test_file
    test_filename
    test_tcp_socket
    test_udp_socket
    notify_init_server
    ping_pong_server
)

pack_archive(test_estdlib_lib DEPENDS_ON eavmlib MODULES ${ERLANG_MODULES})
pack_test(test_estdlib estdlib eavmlib avm_network avm_unix etest)

# Build .beam files for the BEAM peer used in BEAM<->AtomVM serial
# distribution tests.  Only modules that AtomVM provides and OTP does
# not are placed here; OTP's own gen_server, dist_util, etc. are used
# as-is.  serial_dist_controller is compiled with -DBEAM_INTERFACE so
# that recv returns lists (as OTP's dist_util expects).
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/beam_beams/serial_dist_controller.beam
           ${CMAKE_CURRENT_BINARY_DIR}/beam_beams/serial_dist.beam
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/beam_beams
        && erlc +debug_info -DBEAM_INTERFACE
            -I ${CMAKE_SOURCE_DIR}/libs/include
            -o ${CMAKE_CURRENT_BINARY_DIR}/beam_beams
            ${CMAKE_SOURCE_DIR}/libs/estdlib/src/serial_dist_controller.erl
        && ${CMAKE_COMMAND} -E copy
            ${CMAKE_BINARY_DIR}/libs/estdlib/src/beams/serial_dist.beam
            ${CMAKE_CURRENT_BINARY_DIR}/beam_beams/serial_dist.beam
    DEPENDS ${CMAKE_SOURCE_DIR}/libs/estdlib/src/serial_dist_controller.erl
            estdlib
    COMMENT "Building BEAM peer modules for serial distribution tests"
    VERBATIM
)
add_custom_target(
    serial_dist_controller_beam ALL
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/beam_beams/serial_dist_controller.beam
            ${CMAKE_CURRENT_BINARY_DIR}/beam_beams/serial_dist.beam
)

# Build a separate AVM for the serial distribution peer node
if(AVM_DISABLE_JIT)
    set(peer_precompiled_suffix "")
    set(peer_jit_archives "")
    set(peer_jit_targets "")
else()
    set(peer_precompiled_suffix "-${AVM_JIT_TARGET_ARCH}")
    set(peer_jit_archives ${CMAKE_BINARY_DIR}/libs/jit/src/jit${peer_precompiled_suffix}.avm)
    set(peer_jit_targets jit)
endif()

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/test_serial_dist_socat_peer.avm
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/test_estdlib_lib.avm
            ${CMAKE_BINARY_DIR}/libs/estdlib/src/estdlib${peer_precompiled_suffix}.avm
            ${CMAKE_BINARY_DIR}/libs/eavmlib/src/eavmlib.avm
            ${CMAKE_BINARY_DIR}/libs/avm_unix/src/avm_unix.avm
            ${peer_jit_archives}
            test_estdlib_lib estdlib eavmlib avm_unix ${peer_jit_targets} PackBEAM
    COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/packbeam create
        -s test_serial_dist_socat_peer
        ${CMAKE_CURRENT_BINARY_DIR}/test_serial_dist_socat_peer.avm
        ${CMAKE_CURRENT_BINARY_DIR}/test_estdlib_lib.avm
        ${peer_jit_archives}
        ${CMAKE_BINARY_DIR}/libs/estdlib/src/estdlib${peer_precompiled_suffix}.avm
        ${CMAKE_BINARY_DIR}/libs/eavmlib/src/eavmlib.avm
        ${CMAKE_BINARY_DIR}/libs/avm_unix/src/avm_unix.avm
    COMMENT "Packing peer AVM for serial distribution tests"
    VERBATIM
)
add_custom_target(
    test_serial_dist_socat_peer ALL
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/test_serial_dist_socat_peer.avm
)

