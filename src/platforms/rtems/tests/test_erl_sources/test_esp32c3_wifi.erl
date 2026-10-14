%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M. <petermm@gmail.com>
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

-module(test_esp32c3_wifi).
-export([start/0]).

start() ->
    erlang:display({atomvm_esp32c3_wifi, atomvm:platform()}),
    wifi_smoke(),
    socket_smoke(),
    http_smoke(),
    erlang:display({wifi_test_complete, ok}),
    ok.

wifi_smoke() ->
    Status0 = atomvm_rtems:wifi_status(),
    erlang:display({initial_wifi_status, Status0}),
    case atomvm_rtems:wifi_start("AtomVM-TestAP", "secretpassword") of
        ok ->
            erlang:display({wifi_start, ok}),
            wait_for_wifi(30);
        {error, enotsup} ->
            erlang:display({wifi_start, enotsup}),
            ok;
        Error ->
            erlang:display({wifi_start_result, Error}),
            ok
    end.

wait_for_wifi(0) ->
    erlang:display({wifi_status_after_wait, atomvm_rtems:wifi_status()});
wait_for_wifi(Attempts) ->
    case atomvm_rtems:wifi_status() of
        {ok, Info} ->
            State = proplists:get_value(state, Info),
            case State of
                got_ip ->
                    erlang:display({wifi_status, got_ip}),
                    erlang:display({wifi_ip_info, Info});
                _ ->
                    timer:sleep(500),
                    wait_for_wifi(Attempts - 1)
            end;
        Other ->
            erlang:display({wifi_status, Other}),
            timer:sleep(500),
            wait_for_wifi(Attempts - 1)
    end.

socket_smoke() ->
    try socket:open(inet, stream, tcp) of
        {ok, Sock} ->
            erlang:display({socket_open, ok}),
            ok = socket:close(Sock),
            erlang:display({socket_close, ok});
        {error, Reason} ->
            erlang:display({socket_open, error, Reason});
        Other ->
            erlang:display({socket_open, other, Other})
    catch
        error:undef ->
            erlang:display({socket, undef})
    end.

http_smoke() ->
    try
        erlang:display(starting_http_test),
        case ahttp_client:connect(http, "192.168.4.1", 8080, [{inet_backend, socket}, {active, false}]) of
            {ok, Conn} ->
                erlang:display({http_connected, ok}),
                case ahttp_client:request(Conn, <<"GET">>, <<"/README.md">>, [], undefined) of
                    {ok, Conn2, Ref} ->
                        erlang:display({http_requested, Ref}),
                        case ahttp_client:recv(Conn2, 0) of
                            {ok, Conn3, Responses} ->
                                erlang:display({http_responses, Responses}),
                                ahttp_client:close(Conn3),
                                erlang:display({http_closed, ok});
                            {error, RecvErr} ->
                                erlang:display({http_recv_error, RecvErr}),
                                ahttp_client:close(Conn2)
                        end;
                    {error, ReqErr} ->
                        erlang:display({http_request_error, ReqErr}),
                        ahttp_client:close(Conn)
                end;
            {error, ConnErr} ->
                erlang:display({http_connect_result, ConnErr})
        end
    catch
        Class:Reason:Stack ->
            erlang:display({http_exception, Class, Reason, Stack})
    end.
