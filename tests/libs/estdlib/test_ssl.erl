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
    ok = test_negotiated_tls_version(),
    ok = test_connect_close(),
    ok = test_connect_error(),
    ok = test_send_recv(),
    ok = test_send_recv_zero(),
    ok = ssl:stop(),
    ok.

test_negotiated_tls_version() ->
    % Bound DNS, handshake and response reads: AtomVM's SSL API has no timeout.
    {Pid, Ref} = spawn_monitor(fun check_negotiated_tls_version/0),
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok;
        {'DOWN', Ref, process, Pid, Reason} -> erlang:error({tls_test_failed, Reason})
    after 30000 ->
        exit(Pid, kill),
        erlang:error(tls_test_timeout)
    end.

check_negotiated_tls_version() ->
    Expected = expected_tls_version(),
    {ok, SSLSocket} = ssl:connect("check-tls.akamai.io", 443, [
        {verify, verify_none}, {active, false}, {binary, true}
    ]),
    try
        % HTTP/1.0 avoids chunked transfer encoding and closes after the response.
        ok = ssl:send(SSLSocket, [
            <<"GET /v1/tlsinfo.json HTTP/1.0\r\nHost: check-tls.akamai.io\r\n">>,
            <<"Connection: close\r\n\r\n">>
        ]),
        Response = recv_response(SSLSocket, []),
        [Headers, Body] = binary:split(Response, <<"\r\n\r\n">>),
        [Status | HeaderLines] = binary:split(Headers, <<"\r\n">>, [global]),
        [_, <<"200">>, _] = binary:split(Status, <<" ">>, [global]),
        [Length] = [Value || <<"Content-Length: ", Value/binary>> <- HeaderLines],
        BodySize = binary_to_integer(Length),
        BodySize = byte_size(Body),
        Negotiated = tls_version_field(Body),
        io:format("TLS negotiated ~s; expected ~s~n", [Negotiated, Expected]),
        Expected = Negotiated,
        ok
    after
        ok = ssl:close(SSLSocket)
    end.

tls_version_field(Body) ->
    {Pos, Len} = binary:match(Body, <<"\"tls_version\"">>),
    Rest = binary:part(Body, Pos + Len, byte_size(Body) - Pos - Len),
    [_, Version | _] = binary:split(Rest, <<"\"">>, [global]),
    Version.

recv_response(SSLSocket, Acc) ->
    case ssl:recv(SSLSocket, 0) of
        {ok, Data} -> recv_response(SSLSocket, [Data | Acc]);
        {error, closed} -> iolist_to_binary(lists:reverse(Acc));
        % AtomVM exposes MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY as an integer.
        {error, -16#7880} -> iolist_to_binary(lists:reverse(Acc));
        {error, Reason} -> erlang:error({tls_recv_failed, Reason})
    end.

expected_tls_version() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            case lists:member('tlsv1.3', proplists:get_value(supported, ssl:versions())) of
                true -> <<"tls1.3">>;
                false -> <<"tls1.2">>
            end;
        _ ->
            expected_atomvm_tls_version()
    end.

% The flag reflects the build configuration, not the observed negotiation.
-ifdef(TEST_SSL_TLS13).
expected_atomvm_tls_version() ->
    {<<"mbedtls">>, Version, _} = lists:keyfind(<<"mbedtls">>, 1, crypto:info_lib()),
    if
        Version >= 16#03060100 -> <<"tls1.3">>;
        true -> <<"tls1.2">>
    end.
-else.
expected_atomvm_tls_version() ->
    <<"tls1.2">>.
-endif.

test_start_twice() ->
    ok = ssl:start().

test_connect_close() ->
    {ok, SSLSocket} = ssl:connect("test.atomvm.org", 443, [{verify, verify_none}, {active, false}]),
    ok = ssl:close(SSLSocket).

test_connect_error() ->
    {error, _Error} = ssl:connect("test.atomvm.org", 80, [{verify, verify_none}, {active, false}]),
    ok.

test_send_recv() ->
    {ok, SSLSocket} = ssl:connect("test.atomvm.org", 443, [
        {verify, verify_none}, {active, false}, {binary, true}
    ]),
    UserAgent = erlang:system_info(machine),
    ok = ssl:send(SSLSocket, [
        <<"GET / HTTP/1.1\r\nHost: test.atomvm.org\r\nUser-Agent: ">>, UserAgent, <<"\r\n\r\n">>
    ]),
    {ok, <<"HTTP/1.1 200 OK">>} = ssl:recv(SSLSocket, 15),
    ok = ssl:close(SSLSocket),
    ok.

test_send_recv_zero() ->
    {ok, SSLSocket} = ssl:connect("test.atomvm.org", 443, [
        {verify, verify_none}, {active, false}, {binary, true}
    ]),
    UserAgent = erlang:system_info(machine),
    ok = ssl:send(SSLSocket, [
        <<"GET / HTTP/1.1\r\nHost: test.atomvm.org\r\nUser-Agent: ">>, UserAgent, <<"\r\n\r\n">>
    ]),
    {ok, <<"HTTP/1.1 200 OK", _/binary>>} = ssl:recv(SSLSocket, 0),
    ok = ssl:close(SSLSocket),
    ok.
