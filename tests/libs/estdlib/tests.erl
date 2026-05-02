%
% This file is part of AtomVM.
%
% Copyright 2019-2021 Fred Dushin <fred@dushin.net>
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
%

-module(tests).

-export([start/0]).

start() ->
    OTPVersion = get_otp_version(),
    NonNetworkingTests = get_non_networking_tests(OTPVersion),
    NetworkingTests =
        case OTPVersion of
            atomvm ->
                case atomvm:platform() of
                    emscripten ->
                        [];
                    stm32 ->
                        [];
                    wasi ->
                        %% Only wasm32-wasip2 ships BSD sockets via
                        %% wasi:sockets@0.2.x; wasip1/wasip1-threads do not.
                        case erlang:system_info(system_architecture) of
                            <<"wasm32-wasip2">> -> get_wasip2_networking_tests();
                            _ -> []
                        end;
                    _ ->
                        get_networking_tests()
                end;
            _ ->
                get_networking_tests()
        end,
    ok = etest:test(NonNetworkingTests ++ NetworkingTests).

get_otp_version() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            list_to_integer(erlang:system_info(otp_release));
        _ ->
            atomvm
    end.

% test_sets heavily relies on is_equal that is from OTP-27
% test_json requires json module that is from OTP-27
get_non_networking_tests(OTPVersion) when OTPVersion >= 27 ->
    [test_sets, test_json | get_non_networking_tests(26)];
get_non_networking_tests(_OTPVersion) ->
    [
        test_apply,
        test_binary,
        test_lists,
        test_calendar,
        test_gen_event,
        test_gen_server,
        test_gen_statem,
        test_io_lib,
        test_logger,
        test_maps,
        test_proc_lib,
        test_proplists,
        test_queue,
        test_timer,
        test_spawn,
        test_supervisor,
        test_lists_subtraction,
        test_os,
        test_file,
        test_init,
        test_filename,
        test_serial_dist,
        test_uart,
        test_gpio,
        test_serial_dist_socat
    ].

get_networking_tests() ->
    [
        test_tcp_socket,
        test_udp_socket,
        test_epmd,
        test_net,
        test_ssl,
        test_gen_udp,
        test_gen_tcp,
        test_inet,
        test_net_kernel
    ].

%% Subset of get_networking_tests/0 that runs on wasm32-wasip2. Excluded:
%%   - test_epmd, test_net_kernel: need fork/exec (atomvm:subprocess), unavailable on WASI
%%
%% test_gen_tcp / test_gen_udp gate their inet-port-driver subtests on
%% test_inet:is_inet_driver_available/0 and run only the socket backend on WASI.
get_wasip2_networking_tests() ->
    [
        test_tcp_socket,
        test_udp_socket,
        test_net,
        test_inet,
        test_ssl,
        test_gen_tcp,
        test_gen_udp
    ].
