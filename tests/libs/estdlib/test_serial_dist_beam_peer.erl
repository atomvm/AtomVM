%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

%% @doc BEAM peer node for serial distribution tests.
%%
%% This module runs on BEAM (OTP). It is spawned by AtomVM's
%% test_serial_dist_socat to test BEAM<->AtomVM distribution over
%% socat virtual serial ports.
%%
%% Reads PTY_PATH and TEST_NAME from environment variables.
%% Uses OTP's native net_kernel with `-proto_dist serial' so that
%% serial_dist is used as the distribution protocol. UART options
%% are passed to serial_dist via application env.

-module(test_serial_dist_beam_peer).

-export([start/0]).

start() ->
    PtyPath = os:getenv("PTY_PATH"),
    TestName = os:getenv("TEST_NAME"),
    application:set_env(serial_dist, dist_opts, #{
        uart_opts => [{peripheral, PtyPath}, {speed, 115200}],
        uart_module => file_uart_hal
    }),
    {ok, _} = net_kernel:start('beam_b@serial.local', #{name_domain => longnames}),
    erlang:set_cookie('SerialTest'),
    test_serial_dist_socat_peer:run_test('a@serial.local', TestName).
