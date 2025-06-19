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
    Networking =
        case OTPVersion of
            atomvm ->
                case atomvm:platform() of
                    emscripten ->
                        false;
                    stm32 ->
                        false;
                    _ ->
                        true
                end;
            _ ->
                true
        end,
    NetworkingTests =
        if
            Networking -> get_networking_tests(OTPVersion);
            true -> []
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
get_non_networking_tests(OTPVersion) when
    (is_integer(OTPVersion) andalso OTPVersion >= 27) orelse OTPVersion =:= atomvm
->
    [test_sets | get_non_networking_tests(undefined)];
% test_binary uses encode_hex/1 (OTP-24), encode_hex/2 (OTP-26)
get_non_networking_tests(OTPVersion) when
    (is_integer(OTPVersion) andalso OTPVersion >= 26) orelse OTPVersion =:= atomvm
->
    [test_binary | get_non_networking_tests(undefined)];
get_non_networking_tests(_OTPVersion) ->
    [
        test_apply,
        test_lists,
        test_calendar,
        test_gen_event,
        test_gen_server,
        test_gen_statem,
        test_io_lib,
        test_logger,
        test_maps,
        test_proplists,
        test_queue,
        test_timer,
        test_spawn,
        test_supervisor,
        test_lists_subtraction
    ].

get_networking_tests(OTPVersion) when
    (is_integer(OTPVersion) andalso OTPVersion >= 24) orelse OTPVersion =:= atomvm
->
    [
        test_tcp_socket,
        test_udp_socket,
        test_epmd,
        test_net,
        test_ssl
        | get_networking_tests(undefined)
    ];
get_networking_tests(_OTPVersion) ->
    [test_gen_udp, test_gen_tcp, test_inet, test_net_kernel].
