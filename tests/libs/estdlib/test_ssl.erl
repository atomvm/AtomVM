%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_ssl).

-export([test/0]).

-include("etest.hrl").

test() ->
    case is_ssl_available() of
        true ->
            test_ssl();
        false ->
            io:format("Warning: skipping test_ssl as ssl is not available\n"),
            ok
    end.

is_ssl_available() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            true;
        _ ->
            try
                ssl:nif_init(),
                true
            catch
                error:undef ->
                    false
            end
    end.

test_ssl() ->
    ok = ssl:start(),
    ok = test_start_twice(),
    ok = test_connect_close(),
    ok = test_connect_error(),
    ok = test_send_recv(),
    ok = ssl:stop(),
    ok.

test_start_twice() ->
    ok = ssl:start().

test_connect_close() ->
    {ok, SSLSocket} = ssl:connect("atomvm.net", 443, [{verify, verify_none}, {active, false}]),
    ok = ssl:close(SSLSocket).

test_connect_error() ->
    {error, _Error} = ssl:connect("atomvm.net", 80, [{verify, verify_none}, {active, false}]),
    ok.

test_send_recv() ->
    {ok, SSLSocket} = ssl:connect("atomvm.net", 443, [
        {verify, verify_none}, {active, false}, {binary, true}
    ]),
    UserAgent = erlang:system_info(machine),
    ok = ssl:send(SSLSocket, [
        <<"GET / HTTP/1.1\r\nHost: atomvm.net\r\nUser-Agent: ">>, UserAgent, <<"\r\n\r\n">>
    ]),
    {ok, <<"HTTP/1.1">>} = ssl:recv(SSLSocket, 8),
    ok = ssl:close(SSLSocket),
    ok.
