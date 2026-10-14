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
    erlang:display({wifi_test_complete, ok}),
    ok.

wifi_smoke() ->
    Status0 = atomvm_rtems:wifi_status(),
    erlang:display({initial_wifi_status, Status0}),
    case atomvm_rtems:wifi_start("AtomVM-TestAP", "secretpassword") of
        ok ->
            erlang:display({wifi_start, ok}),
            Status1 = atomvm_rtems:wifi_status(),
            erlang:display({started_wifi_status, Status1}),
            ok;
        {error, enotsup} ->
            erlang:display({wifi_start, enotsup}),
            ok;
        Error ->
            erlang:display({wifi_start_result, Error}),
            ok
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
